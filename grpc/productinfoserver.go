package main

import (
  "fmt"
  "strings"
  "strconv"
  "time"
  "math/rand"
  "context"
  "io"
  "os"
  "net"
  "google.golang.org/grpc"
  "google.golang.org/grpc/codes"
  "google.golang.org/grpc/status"
  wr "google.golang.org/protobuf/types/known/wrapperspb"
  ec "grpctest/ecommerce"
)

type server struct {
  ec.UnimplementedProductInfoServer
  store map[string]*ec.Product
}

// request-response: AddProduct(ctx)
// - inbound: method invoked
// - outbound: return value
func (s *server) AddProduct(
  ctx context.Context, prd *ec.Product,
) (*ec.ProductID, error) {
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
func (s *server) GetProduct(
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
func (s *server) SearchProducts(
  query *wr.StringValue, stream ec.ProductInfo_SearchProductsServer,
) error {
  for _, prd := range s.store {
    if strings.Contains(prd.Desc, query.Value) {
      err := stream.Send(prd)
      if err != nil {
        return err
      }
      time.Sleep(500 * time.Millisecond)
    }
  }
  return nil
}

// client streaming: UpdateProducts(stream)
// - inbound: stream.Recv(), EOF, stream.SendAndClose()
// - outbound: EOF, stream.SendAndClose()
func (s *server) UpdateProducts(
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
func (s *server) GetProducts(stream ec.ProductInfo_GetProductsServer) error {
  for {
    id, err := stream.Recv()
    if err != nil {
      if err == io.EOF {
        return nil
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

func logUnaryInterceptor(
  ctx context.Context, req any,
  info *grpc.UnaryServerInfo, handler grpc.UnaryHandler,
) (any, error) {
  start := time.Now() // pre-processing
  res, err := handler(ctx, req) // forwarding
  // post-processing
  fmt.Printf("unary %v %v\n", info.FullMethod, time.Since(start))
  return res, err
}

func logStreamInterceptor(
  srv any, sstream grpc.ServerStream,
  info *grpc.StreamServerInfo, handler grpc.StreamHandler,
) error {
  start := time.Now() // pre-processing
  err := handler(srv, sstream)
  fmt.Printf("stream %v %v\n", info.FullMethod, time.Since(start))
  return err
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
  srv := grpc.NewServer(
    grpc.UnaryInterceptor(logUnaryInterceptor),
    grpc.StreamInterceptor(logStreamInterceptor),
  )
  ec.RegisterProductInfoServer(srv, &server{})
  err = srv.Serve(lis)
  exitOnError(err)
}
