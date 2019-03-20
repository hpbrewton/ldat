package main

import (
  "fmt"
  "math"
)

func boundary (start float64, orcl func(float64)bool) float64 {
  left := start
  right := math.MaxFloat64
  for mid := (right + left)/2;
  right != mid && left != mid;
  mid = (right + left)/2 {
    if orcl(mid) {
      left = mid
    } else {
      right = mid
    }
  }
  if orcl(right) {
    return right
  } else {
    return left
  }
}

func interval (min, max float64, orcl func(float64)bool) (float64, float64) {
  left := -boundary(min, func(n float64)bool {
    return orcl(-n)
  })
  right := boundary(max, orcl)
  return left, right
}

func lists (example []float64, orcl func([]float64)bool) {
  if len(example) == 0 {
    fmt.Println("empty list")
    return
  }
  minimum := example[0]
  maximum := example[0]
  for _, v := range example {
    if v < minimum {
      minimum = v
    }
    if v > maximum {
      maximum = v
    }
  }
  l, r := interval(minimum, maximum, func(f float64)bool  {
    example[0] = f
    return orcl(example)
  })
  fmt.Println(l, r)
}

func test (vec []float64) bool {
  for _, v := range vec {
    if v <= -80.0 || v >= 90.001{
      return false
    }
  }
  return true
}

func main ()  {
  lists([]float64{90, 10, -70}, test)
}
