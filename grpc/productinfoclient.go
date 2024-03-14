package main

import (
  "fmt"
  "time"
  "context"
  "io"
  "os"
  "sync"
  "google.golang.org/grpc"
  wr "google.golang.org/protobuf/types/known/wrapperspb"
  ec "grpctest/ecommerce"
)

// request-response: AddProduct(ctx), GetProduct(ctx)
// - outbound: local call
// - inbound: return value
func requestResponse(ctx context.Context, cln ec.ProductInfoClient) {
  fmt.Println("* Request-response: AddProduct(), GetProduct()")
  prd := &ec.Product{Name: "Nextra", Desc: "Bugari bayan", Price: 16e3}
  id, err := cln.AddProduct(ctx, prd)
  exitOnError(err)
  fmt.Printf("Product Id %v\n", id.Value)
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
  return prds
}

// client streaming: UpdateProducts() => stream
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

// bidirectional streaming: GetProducts() => stream
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

func exitOnError(err error) {
  if err != nil {
    fmt.Println(err)
    os.Exit(1)
  }
}

func main() {
  conn, err := grpc.Dial("localhost:4321", grpc.WithInsecure())
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
