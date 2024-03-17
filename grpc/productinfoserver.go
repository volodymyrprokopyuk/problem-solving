package main

import (
	"context"
	"crypto/tls"
	"encoding/base64"

	// "crypto/x509"
	"fmt"
	ec "grpctest/ecommerce"
	"io"
	"math/rand"
	"net"
	"os"
	"strconv"
	"strings"
	"time"

	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/credentials"
	"google.golang.org/grpc/metadata"
	"google.golang.org/grpc/status"
  "google.golang.org/grpc/reflection"
	wr "google.golang.org/protobuf/types/known/wrapperspb"
)

type prdService struct {
  store map[string]*ec.Product
  ec.UnimplementedProductInfoServer
}

// request-response: AddProduct(ctx)
// - inbound: method invoked
// - outbound: return value
func (s *prdService) AddProduct(
  ctx context.Context, prd *ec.Product,
) (*ec.ProductID, error) {
  // read client metadata from context
  if meta, exist := metadata.FromIncomingContext(ctx); exist {
    fmt.Printf("meta %v\n", meta)
  }
  // write server metadata on context
  _ = grpc.SendHeader(ctx, metadata.Pairs("srv1", "srvHeader"))
  _ = grpc.SetTrailer(ctx, metadata.Pairs("svr2", "svrTrailer"))
  if s.store == nil {
    s.store = make(map[string]*ec.Product, 10)
  }
  prd.Id = strconv.Itoa(rand.Intn(1000))
  s.store[prd.Id] = prd
  return &ec.ProductID{Value: prd.Id}, status.New(codes.OK, "").Err()
}

// request-response: GetProduct(ctx)
// - inbound: method invoked
// - outbound: return value
func (s *prdService) GetProduct(
  ctx context.Context, id *ec.ProductID,
) (*ec.Product, error) {
  if prd, exist := s.store[id.Value]; exist {
    return prd, status.New(codes.OK, "").Err()
  }
  return nil, status.Errorf(
    codes.NotFound, "Product %v does not exist", id.Value,
  )
}

// server streaming: SearchProducts(stream)
// - inbound: method invoked
// - outbound: stream.Send(), return nil
func (s *prdService) SearchProducts(
  query *wr.StringValue, stream ec.ProductInfo_SearchProductsServer,
) error {
  // read client metadata from context
  if meta, exist := metadata.FromIncomingContext(stream.Context()); exist {
    fmt.Printf("meta %v\n", meta)
  }
  // write server metadata to context
  stream.SendHeader(metadata.Pairs("srv1", "srvHeader"))
  stream.SetTrailer(metadata.Pairs("svf2", "srvTrailer"))
  for _, prd := range s.store {
    if strings.Contains(prd.Desc, query.Value) {
      err := stream.Send(prd)
      if err != nil {
        return err
      }
      time.Sleep(500 * time.Millisecond)
    }
  }
  return status.New(codes.OK, "").Err()
}

// client streaming: UpdateProducts(stream)
// - inbound: stream.Recv(), EOF, stream.SendAndClose()
// - outbound: EOF, stream.SendAndClose()
func (s *prdService) UpdateProducts(
  stream ec.ProductInfo_UpdateProductsServer,
) error {
  ids := make([]string, 0, 10)
  for {
    prd, err := stream.Recv()
    if err != nil {
      if err == io.EOF {
        return stream.SendAndClose(
          &wr.StringValue{Value: strings.Join(ids, ", ")},
        )
      }
      return err
    }
    s.store[prd.Id] = prd
    ids = append(ids, prd.Id)
  }
}

// bidirectional streaming: GetProducts(stream)
// - inbound: stream.Recv(), EOF, return nil
// - outbound: stream.Send(), return nil
func (s *prdService) GetProducts(stream ec.ProductInfo_GetProductsServer) error {
  for {
    id, err := stream.Recv()
    if err != nil {
      if err == io.EOF {
        return status.New(codes.OK, "").Err()
      }
      return err
    }
    if prd, exist := s.store[id.Value]; exist {
      err := stream.Send(prd)
      if err != nil {
        return err
      }
    } else {
      return status.Errorf(
        codes.NotFound, "Product %v does not exist", id.Value,
      )
    }
  }
}

func srvLogUnaryInterceptor(
  ctx context.Context, req any,
  info *grpc.UnaryServerInfo, handler grpc.UnaryHandler,
) (any, error) {
  start := time.Now() // pre-processing
  res, err := handler(ctx, req) // forwarding
  // post-processing
  fmt.Printf("unary %v %v\n", info.FullMethod, time.Since(start))
  return res, err
}

type wrServerStream struct {
  grpc.ServerStream
}

func (ss *wrServerStream) RecvMsg(msg any) error {
  fmt.Printf("  stream receive %v\n", msg)
  return ss.ServerStream.RecvMsg(msg)
}

func (ss *wrServerStream) SendMsg(msg any) error {
  fmt.Printf("  stream send %v\n", msg)
  return ss.ServerStream.SendMsg(msg)
}

func srvLogStreamInterceptor(
  srv any, ss grpc.ServerStream,
  info *grpc.StreamServerInfo, handler grpc.StreamHandler,
) error {
  start := time.Now() // pre-processing
  err := handler(srv, &wrServerStream{ss}) // forwarding
  // post-processing
  fmt.Printf("stream %v %v\n", info.FullMethod, time.Since(start))
  return err
}

func basicAuthNInterceptor(
  ctx context.Context, req any,
  info *grpc.UnaryServerInfo, handler grpc.UnaryHandler,
) (any, error) {
  meta, exist := metadata.FromIncomingContext(ctx)
  if !exist {
    return nil, status.Errorf(
      codes.Unauthenticated, "cannot get metadata from context",
    )
  }
  auth, exist := meta["authorization"]
  if !exist {
    return nil, status.Errorf(
      codes.Unauthenticated, "missing authorization header",
    )
  }
  provided := strings.TrimPrefix(auth[0], "Basic ")
  expected := base64.StdEncoding.EncodeToString([]byte("cln1:secret1"))
  fmt.Printf("%v %v\n", provided, expected)
  if provided != expected {
    return nil, status.Errorf(
      codes.Unauthenticated, "invalid credentials",
    )
  }
  return handler(ctx, req)
}

func exitOnError(err error) {
  if err != nil {
    fmt.Println(err)
    os.Exit(1)
  }
}

func main() {
  // * server TLS
  cert, err := tls.LoadX509KeyPair("srvcert.pem", "srvkey.pem")
  exitOnError(err)

  // + mutual TLS
  // certPool := x509.NewCertPool()
  // cacert, err := os.ReadFile("cacert.pem")
  // exitOnError(err)
  // if !certPool.AppendCertsFromPEM(cacert) {
  //   fmt.Println("cannot append cacert to cert pool")
  //   os.Exit(1)
  // }

  listener, err := net.Listen("tcp", ":4321")
  exitOnError(err)
  server := grpc.NewServer(
    // * server TLS
    grpc.Creds(credentials.NewServerTLSFromCert(&cert)),

    // * mutual TLS
    // grpc.Creds(credentials.NewTLS(&tls.Config{
    //   Certificates: []tls.Certificate{cert},
    //   ClientAuth: tls.RequireAndVerifyClientCert,
    //   ClientCAs: certPool,
    // })),

    // * basic authentication
    grpc.UnaryInterceptor(basicAuthNInterceptor),

    // grpc.UnaryInterceptor(srvLogUnaryInterceptor),
    // grpc.StreamInterceptor(srvLogStreamInterceptor),
  )
  ec.RegisterProductInfoServer(server, &prdService{})
  reflection.Register(server) // enable gRPC reflection (optional)
  err = server.Serve(listener)
  exitOnError(err)
}
