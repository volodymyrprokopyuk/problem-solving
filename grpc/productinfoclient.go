package main

import (
  "fmt"
  "time"
  "context"
  "os"
  pb "grpctest/ecommerce"
  "google.golang.org/grpc"
)

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
  cln := pb.NewProductInfoClient(conn)
  ctx, cancel := context.WithTimeout(context.Background(), 1 * time.Second)
  defer cancel()
  prd := &pb.Product{Name: "Nextra", Description: "Bugary bayan", Price: 16e3}
  id, err := cln.AddProduct(ctx, prd)
  exitOnError(err)
  fmt.Printf("Product Id %v\n", id.Value)
  prd, err = cln.GetProduct(ctx, id)
  exitOnError(err)
  fmt.Printf("Product %v\n", prd)
}
