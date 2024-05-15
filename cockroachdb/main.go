package main

import (
	"database/sql"
	"fmt"
	"os"

	_ "github.com/jackc/pgx/v5/stdlib"
)

func oneRow(db *sql.DB) error {
  var num int
  var str string
  err := db.QueryRow("SELECT 1, 'a';").Scan(&num, &str)
  if err != nil {
    return err
  }
  fmt.Println(num, str)
  return nil
}

func multiRow(db *sql.DB) error {
  rows, err := db.Query("VALUES (1, 'a'), (2, 'b');")
  if err != nil {
    return err
  }
  var num int
  var str string
  for rows.Next() {
    err = rows.Scan(&num, &str)
    if err != nil {
      return err
    }
    fmt.Println(num, str)
  }
  return nil
}

func noRow(db *sql.DB) error {
  res, err := db.Exec("SET database = learning;")
  if err != nil {
    return err
  }
  count, err := res.RowsAffected()
  if err != nil {
    return err
  }
  fmt.Println(count)
  return nil
}

func exitOnError(err error) {
  if err != nil {
    fmt.Println(err)
    os.Exit(1)
  }
}

func main() {
  url := "postgresql://root@localhost:26257/learning?sslmode=disable"
  db, err := sql.Open("pgx", url)
  exitOnError(err)
  defer db.Close()
  // err = oneRow(db)
  // err = multiRow(db)
  err = noRow(db)
  exitOnError(err)
}
