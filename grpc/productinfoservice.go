package main

import (
  "fmt"
  "strconv"
  "math/rand"
  "context"
  "google.golang.org/grpc"
  "google.golang.org/grpc/codes"
  "google.golang.org/grpc/status"
  pb "grpctest/ecommerce"
  "os"
  "net"
)

type server struct {
  pb.UnimplementedProductInfoServer
  store map[string]*pb.Product
}

func (s *server) AddProduct(
  ctx context.Context, prd *pb.Product,
) (*pb.ProductID, error) {
  if s.store == nil {
    s.store = make(map[string]*pb.Product, 10)
  }
  prd.Id = strconv.Itoa(rand.Intn(1000))
  s.store[prd.Id] = prd
  return &pb.ProductID{Value: prd.Id}, status.New(codes.OK, "").Err()
}

func (s *server) GetProduct(
  ctx context.Context, id *pb.ProductID,
) (*pb.Product, error) {
  if prd, exist := s.store[id.Value]; exist {
    return prd, status.New(codes.OK, "").Err()
  }
  return nil, status.Errorf(
    codes.NotFound, "Product %v does not exist", id.Value,
  )
}

func exitOnError(err error) {
  if err != nil {
    fmt.Println(err)
    os.Exit(1)
  }
}

func main() {
  lis, err := net.Listen("tcp", ":4321")
  exitOnError(err)
  srv := grpc.NewServer()
  pb.RegisterProductInfoServer(srv, &server{})
  err = srv.Serve(lis)
  exitOnError(err)
}
