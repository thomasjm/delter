# Hello World - Final Version

This is a comprehensive Markdown document for testing - **version 3**.

## Introduction

Welcome to **Markdown**! This is the final version of our test document with many features.

We can write:
- Comprehensive lists with more items
- *Italic text* and _emphasized text_
- `Code snippets` and inline code
- **Bold text** and __strong text__
- ~~Strikethrough text~~
- [Links to websites](https://example.com)

### Mathematical Expressions

$$\sum_{i=1}^n i = \frac{n(n+1)}{2}$$

Complex equations:
$$\int_0^\infty e^{-x^2} dx = \frac{\sqrt{\pi}}{2}$$

$$\lim_{n \to \infty} \left(1 + \frac{1}{n}\right)^n = e$$

### Tables and Code

| Name   | Value | Category | Description      |
|--------|-------|----------|------------------|
| First  | 1     | Odd      | The first number |
| Second | 2     | Even     | The second number|
| Third  | 3     | Odd      | The third number |
| Fourth | 4     | Even     | The fourth number|

```python
def fibonacci(n):
    """Calculate the nth Fibonacci number."""
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)

def factorial(n):
    """Calculate n factorial."""
    if n <= 1:
        return 1
    return n * factorial(n-1)

# Example usage
print(f"Fibonacci(10): {fibonacci(10)}")
print(f"Factorial(5): {factorial(5)}")
```

```bash
# Shell commands
echo "Hello, World!"
ls -la
cat file.txt | grep "pattern"
```

## Conclusion

This demonstrates the evolution of our Markdown document through multiple versions.

> This is a blockquote to show additional formatting capabilities.

---

# Appendix

Additional content on a new page for more substantial differences.

## Additional Features

- [ ] Unchecked task
- [x] Completed task
- [ ] Another unchecked task

### Nested Lists

1. First item
   - Sub-item A
   - Sub-item B
     - Nested sub-item
2. Second item
   1. Numbered sub-item
   2. Another numbered sub-item
