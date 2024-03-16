package main

import (
	"crypto/tls"
	"crypto/x509"
	"context"
	"fmt"
	ec "grpctest/ecommerce"
	"io"
	"os"
	"sync"
	"time"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials"
	"google.golang.org/grpc/metadata"
	wr "google.golang.org/protobuf/types/known/wrapperspb"
)

// request-response: AddProduct(ctx), GetProduct(ctx)
// - outbound: local call
// - inbound: return value
func requestResponse(ctx context.Context, cln ec.ProductInfoClient) {
  fmt.Println("* Request-response: AddProduct(), GetProduct()")
  // write client metadata on context
  ctx = metadata.AppendToOutgoingContext(ctx, "cln1", "clnMeta")
  // read server metadata from context
  var srvHeader, srvTrailer metadata.MD
  prd := &ec.Product{Name: "Nextra", Desc: "Bugari bayan", Price: 16e3}
  id, err := cln.AddProduct(
    ctx, prd, grpc.Header(&srvHeader), grpc.Trailer(&srvTrailer),
  )
  exitOnError(err)
  fmt.Printf("Product Id %v\n", id.Value)
  fmt.Printf("meta header %v\nmeta trailer %v\n", srvHeader, srvTrailer)
  prd, err = cln.GetProduct(ctx, id)
  exitOnError(err)
  fmt.Printf("Product %v\n", prd)
}

// server streaming: SearchProducts(ctx)
// - outbound: local call
// - inbound stream.Recv(), EOF, return nil
func serverStreaming(
  ctx context.Context, cln ec.ProductInfoClient,
) []*ec.Product {
  fmt.Println("* Server streaming: SearchProducts()")
  // write client metadata to context
  ctx = metadata.AppendToOutgoingContext(ctx, "cl1", "clnMeta")
  stream, err := cln.SearchProducts(ctx, &wr.StringValue{Value: "bayan"})
  exitOnError(err)
  prds := make([]*ec.Product, 0, 10)
  for {
    prd, err := stream.Recv()
    if err != nil {
      if err == io.EOF {
        break
      }
      exitOnError(err)
    }
    fmt.Printf("Product %v\n", prd)
    prds = append(prds, prd)
  }
  // read server metadata from context
  srvHeader, _ := stream.Header()
  srvTrailer := stream.Trailer()
  fmt.Printf("meta header %v\nmeta trailer %v\n", srvHeader, srvTrailer)
  return prds
}

// client streaming: UpdateProducts(ctx) => stream
// - outbound: stream.Send(), stream.CloseAndRecv()
// - inbound: stream.CloseAndRecv()
func clientStreaming(
  ctx context.Context, cln ec.ProductInfoClient, prds []*ec.Product,
) {
  fmt.Println("* Client streaming: UpdateProducts()")
  stream, err := cln.UpdateProducts(ctx)
  exitOnError(err)
  for _, prd := range prds {
    prd.Price += 1
    err := stream.Send(prd)
    exitOnError(err)
  }
  updStaus, err := stream.CloseAndRecv()
  exitOnError(err)
  fmt.Printf("Update status %v\n", updStaus.Value)
}

// bidirectional streaming: GetProducts(ctx) => stream
// - outbound: stream.Send(), stream.CloseSend()
// - inbound: goroutine stream.Recv(), EOF, return nil
func bidirectionalStreaming(
  ctx context.Context, cln ec.ProductInfoClient, prds []*ec.Product,
) {
  fmt.Println("* Bidirectional streaming: GetProducts()")
  stream, err := cln.GetProducts(ctx)
  exitOnError(err)
  var wg sync.WaitGroup
  wg.Add(1)
  go func() {
    defer wg.Done()
    for {
      prd, err := stream.Recv()
      if err != nil {
        if err == io.EOF {
          return
        }
        fmt.Println(err)
        return
      }
      fmt.Printf("Product %v\n", prd)
    }
  }()
  for _, prd := range prds {
    err := stream.Send(&ec.ProductID{Value: prd.Id})
    exitOnError(err)
  }
  err = stream.CloseSend()
  exitOnError(err)
  wg.Wait()
}

func clnLogUnaryInterceptor(
  ctx context.Context, method string, req, res any,
  cc *grpc.ClientConn, invoker grpc.UnaryInvoker, opts ...grpc.CallOption,
) error {
  start := time.Now()
  err := invoker(ctx, method, req, res, cc, opts...)
  fmt.Printf("unary %v %v\n", method, time.Since(start))
  return err
}

type wrClientStream struct {
  grpc.ClientStream
}

func (cs *wrClientStream) RecvMsg(msg any) error {
  fmt.Printf("  stream receive %v\n", msg)
  return cs.ClientStream.RecvMsg(msg)
}

func (cs *wrClientStream) SendMsg(msg any) error {
  fmt.Printf("  stream send %v\n", msg)
  return cs.ClientStream.SendMsg(msg)
}

func clnLogStreamInterceptor(
  ctx context.Context, desc *grpc.StreamDesc,
  cc *grpc.ClientConn, method string, streamer grpc.Streamer,
  opts ...grpc.CallOption,
) (grpc.ClientStream, error) {
  fmt.Printf("stream %v\n", method)
  cstream, err := streamer(ctx, desc, cc, method, opts...)
  return &wrClientStream{cstream}, err
}

func exitOnError(err error) {
  if err != nil {
    fmt.Println(err)
    os.Exit(1)
  }
}

func main() {
  // * server TLS
  // creds, err := credentials.NewClientTLSFromFile("srvcert.pem", "localhost")
  // exitOnError(err)

  // * mutula TLS
  cert, err := tls.LoadX509KeyPair("clncert.pem", "clnkey.pem")
  exitOnError(err)
  certPool := x509.NewCertPool()
  cacert, err := os.ReadFile("cacert.pem")
  exitOnError(err)
  if !certPool.AppendCertsFromPEM(cacert) {
    fmt.Println("cannot append cacert to cert pool")
    os.Exit(1)
  }

  conn, err := grpc.Dial(
    "localhost:4321",
    // grpc.WithInsecure(),

    // * server TLS
    // grpc.WithTransportCredentials(creds),

    // * mutual TLS
    grpc.WithTransportCredentials(credentials.NewTLS(&tls.Config{
      Certificates: []tls.Certificate{cert},
      ServerName: "localhost",
      RootCAs: certPool,
    })),

    // grpc.WithUnaryInterceptor(clnLogUnaryInterceptor),
    // grpc.WithStreamInterceptor(clnLogStreamInterceptor),
  )
  exitOnError(err)
  defer conn.Close()
  cln := ec.NewProductInfoClient(conn)
  ctx, cancel := context.WithTimeout(context.Background(), 30 * time.Second)
  defer cancel()

  requestResponse(ctx, cln)
  prds := serverStreaming(ctx, cln)
  clientStreaming(ctx, cln, prds)
  bidirectionalStreaming(ctx, cln, prds)
}
