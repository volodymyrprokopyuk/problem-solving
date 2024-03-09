package main

import (
  "fmt"
  "time"
  "os"
  "net/http"
  "gopkg.in/yaml.v3"
)

type HeadClientConfig struct {
  HeadHost string `yaml:"headHost"`
  ErrorInterval time.Duration `yaml:"errorInterval"`
  HeadInterval time.Duration `yaml:"headInterval"`
}

func configure() (HeadClientConfig, error) {
  configFile := os.Getenv("HEADCLIENT_CONFIG")
  if len(configFile) == 0 {
    configFile = "headclient.yaml"
  }
  file, err := os.Open(configFile)
  if err != nil {
    return HeadClientConfig{}, err
  }
  defer file.Close()
  dec := yaml.NewDecoder(file)
  var config struct { HeadClient HeadClientConfig `yaml:"headClient"` }
  err = dec.Decode(&config)
  if err != nil {
    return HeadClientConfig{}, err
  }
  headHost := os.Getenv("HEADCLIENT_HOST")
  if len(headHost) != 0 {
    config.HeadClient.HeadHost = headHost
  }
  return config.HeadClient, nil
}

func main() {
  config, err := configure()
  if err != nil {
    fmt.Println(err)
    os.Exit(1)
  }
  fmt.Printf("config: %+v\n", config)
  for {
    res, err := http.Head(config.HeadHost)
    if err != nil {
      fmt.Println(err)
      time.Sleep(config.ErrorInterval)
      continue
    }
    server, _ := res.Header["Server"]
    fmt.Printf("%v %v %v\n", time.Now().Format("15:04:05"), res.Status, server)
    time.Sleep(config.HeadInterval)
  }
}
