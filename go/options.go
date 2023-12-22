package main

import "fmt"

/* Builder pattern */
type Config struct { Port int } // always initialized
type Builder struct { port *int } // optional value

func (b *Builder) Port(port int) *Builder {
  b.port = &port
  return b // chaining
}

func (b *Builder) Build() (Config, error) {
  config := Config{}
  if b.port == nil {
    config.Port = 8080 // default value
    return config, nil
  }
  if *b.port < 0 {
    return config, fmt.Errorf("invalid port %d", *b.port)
  }
  config.Port = *b.port
  return config, nil
}

func useBuilder() {
  builder := Builder{}
  builder.Port(1234)
  config, err := builder.Build()
  if err != nil {
    fmt.Println(err)
    return
  }
  fmt.Println(config)
}

/* Functional options pattern */
type options struct { port *int } // optional value
type ReadOpt func(opts *options) error

func WithPort(port int) ReadOpt {
  return func(opts *options) error {
    if port < 0 {
      return fmt.Errorf("invalid port %d", port)
    }
    opts.port = &port
    return nil
  }
}

func configure(readOpts ...ReadOpt) (Config, error) {
  opts, config := options{}, Config{}
  for _, readOpt := range readOpts {
    if err := readOpt(&opts); err != nil {
      return config, err
    }
  }
  if opts.port == nil {
    config.Port = 8080 // default value
    return config, nil
  }
  config.Port = *opts.port
  return config, nil
}

func useOptions() {
  config, err := configure(WithPort(1234))
  if err != nil {
    fmt.Println(err)
    return
  }
  fmt.Println(config)
}

func main() {
  useBuilder()
  useOptions()
}
