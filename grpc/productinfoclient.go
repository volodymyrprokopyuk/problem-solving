package main

import (
  "fmt"
  "time"
  "context"
  "io"
  "os"
  "google.golang.org/grpc"
  wr "google.golang.org/protobuf/types/known/wrapperspb"
  ec "grpctest/ecommerce"
)

// request-response, local call
func requestResponse(ctx context.Context, cln ec.ProductInfoClient) {
  fmt.Println("* Request-response")
  prd := &ec.Product{Name: "Nextra", Description: "Bugary bayan", Price: 16e3}
  id, err := cln.AddProduct(ctx, prd)
  exitOnError(err)
  fmt.Printf("Product Id %v\n", id.Value)
  prd, err = cln.GetProduct(ctx, id)
  exitOnError(err)
  fmt.Printf("Product %v\n", prd)
}

// server streaming, stream.Recv(), EOF
func serverStreaming(
  ctx context.Context, cln ec.ProductInfoClient,
) []*ec.Product {
  fmt.Println("* Server streaming")
  inStream, err := cln.SearchProducts(ctx, &wr.StringValue{Value: "bayan"})
  exitOnError(err)
  prds := make([]*ec.Product, 0, 10)
  for {
    prd, err := inStream.Recv()
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

// client streaming, stream.Send(), stream.CloseAndRecv()
func clientStreaming(
  ctx context.Context, cln ec.ProductInfoClient, prds []*ec.Product,
) {
  fmt.Println("* Client streaming")
  outStream, err := cln.UpdateProducts(ctx)
  exitOnError(err)
  for _, prd := range prds {
    prd.Description += " updated"
    err := outStream.Send(prd)
    exitOnError(err)
  }
  updStaus, err := outStream.CloseAndRecv()
  exitOnError(err)
  fmt.Printf("Update status %v\n", updStaus.Value)
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
}
