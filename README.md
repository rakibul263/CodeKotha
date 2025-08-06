# 💬 CodeKotha - বাংলা প্রোগ্রামিং ল্যাঙ্গুয়েজ

**CodeKotha** একটি ✨ **কম্প্যাক্ট বাংলা প্রোগ্রামিং ল্যাঙ্গুয়েজ কম্পাইলার** যেখানে মাত্র **একটি ফাইলে** পুরো কম্পাইলার সাজানো হয়েছে।

---

## 🚀 দ্রুত শুরু

### ✅ কোড কীভাবে রান করতে হয়

#### 1. কম্পাইল করুন

```bash
# C++ কম্পাইলার দিয়ে কম্পাইল
g++ -std=c++17 -Wall -Wextra -O2 -o codekotha codekotha.cpp

# অথবা Makefile
make -f Makefile
```

#### 2. প্রোগ্রাম রান করুন

```bash
# ডিফল্ট ইনপুট: input.txt → output.txt
./codekotha

# কাস্টম ইনপুট/আউটপুট
./codekotha -i my_input.txt -o my_output.txt

# C কোড জেনারেট করুন
./codekotha -c -o output.c

# সাহায্য
./codekotha -h
```

#### 3. সহজ স্ক্রিপ্ট ব্যবহার

```bash
./compile.sh
```

---

## 📁 প্রজেক্ট স্ট্রাকচার

```
codekotha.cpp           # 🎯 মূল কম্পাইলার
Makefile                # 🔧 বিল্ড টুল
compile.sh              # 🚀 স্ক্রিপ্ট চালানোর জন্য
input.txt               # 📝 ইনপুট কোড
output.txt              # 📄 আউটপুট রেজাল্ট
output_c.txt            # 📄 জেনারেট করা C কোড
sample.kotha            # 📝 টেস্ট প্রোগ্রাম
README.md               # 📖 নির্দেশিকা
```

---

## 🌟 ফিচারস

### 🔢 বাংলা কীওয়ার্ড

* `পূর্ণসংখ্যা` — int declaration
* `যদি` — if
* `নইলে` — else
* `দেখাও` — print

### 🔣 বাংলা সংখ্যা

* `০-৯` — স্বয়ংক্রিয়ভাবে ইংরেজি সংখ্যায় রূপান্তরিত হয়

### ⚖️ অপারেটর

* `+`, `-`, `*`, `/` — গাণিতিক অপারেশন
* `>`, `<` — তুলনা
* `=` — অ্যাসাইনমেন্ট

---

## 📋 উদাহরণ

### 1. ভেরিয়েবল ও প্রিন্ট

```codekotha
পূর্ণসংখ্যা x = ৫;
দেখাও(x);
```

**আউটপুট:**

```
Variable 'x' declared with value: 5
Output: 5
```

### 2. গাণিতিক অপারেশন

```codekotha
পূর্ণসংখ্যা x = ১০;
পূর্ণসংখ্যা y = ৩;
দেখাও(x + y);
দেখাও(x - y);
দেখাও(x * y);
দেখাও(x / y);
```

**আউটপুট:**

```
Variable 'x' declared with value: 10
Variable 'y' declared with value: 3
Output: 13
Output: 7
Output: 30
Output: 3
```

### 3. তুলনা

```codekotha
পূর্ণসংখ্যা x = ১০;
পূর্ণসংখ্যা y = ৩;
দেখাও(x > y);
দেখাও(x < y);
```

**আউটপুট:**

```
Variable 'x' declared with value: 10
Variable 'y' declared with value: 3
Output: 1
Output: 0
```

### 4. কন্ডিশনাল

```codekotha
পূর্ণসংখ্যা x = ১০;
যদি (x > ৫) {
    দেখাও(x);
} নইলে {
    দেখাও(০);
}
```

**আউটপুট:**

```
Variable 'x' declared with value: 10
Condition is true, executing if block...
Output: 10
```

### 5. একাধিক প্যারামিটার

```codekotha
পূর্ণসংখ্যা x = ৫;
পূর্ণসংখ্যা y = ৬;
দেখাও(x, y);
```

**আউটপুট:**

```
Variable 'x' declared with value: 5
Variable 'y' declared with value: 6
Output: 5 6
```

---

## 🔧 কমান্ড লাইন অপশন

```bash
./codekotha [options]

Options:
  -i <file>     ইনপুট ফাইল (default: input.txt)
  -o <file>     আউটপুট ফাইল (default: output.txt)
  -c            C কোড জেনারেট করুন
  -h, --help    সাহায্য দেখুন
```

---

## 📄 আউটপুট ফাইল

### output.txt

```
// CodeKotha Direct Execution Output
// ================================
Program Output:
==============
1
0
13
```

### output\_c.txt

```c
// Generated C Code from CodeKotha
#include <stdio.h>
#include <stdlib.h>

int main() {
    int x = 10;
    int y = 3;
    printf("%d\n", 1);
    printf("%d\n", 0);
    printf("%d\n", 13);
    return 0;
}
```

---

## 🛠️ ইনস্টলেশন

### প্রয়োজনীয়তা

* C++17 কম্পাইলার
* Make (ঐচ্ছিক)

### স্টেপ

```bash
git clone <repo-url>
cd CodeKotha
g++ -std=c++17 -Wall -Wextra -O2 -o codekotha codekotha.cpp
./codekotha
```

---

## 🧪 টেস্টিং

### বেসিক টেস্ট

```bash
echo "পূর্ণসংখ্যা x = ৫; দেখাও(x);" > input.txt
./codekotha
cat output.txt
```

### C কোড টেস্ট

```bash
./codekotha -c -o test.c
gcc test.c -o test_program
./test_program
```

---

## 📝 সিনট্যাক্স রুলস

### ভেরিয়েবল

```codekotha
পূর্ণসংখ্যা <name> = <value>;
```

### প্রিন্ট

```codekotha
দেখাও(<expression>);
দেখাও(<exp1>, <exp2>);
```

### শর্ত

```codekotha
যদি (<condition>) {
  ...
} নইলে {
  ...
}
```

### এক্সপ্রেশন

```
+ - * / > <
```

---

## 😞 সমস্যা সমাধান

### কমন এরর

* C++17 সাপোর্ট চেক করুন
* input.txt আছে কিনা দেখুন
* সেমিকোলন ভুলবেন না ;

### ডিবাগিং

```bash
./codekotha -v
./codekotha -c -o debug.c
cat debug.c
```

---

## 🤝 কন্ট্রিবিউশন

1. ইস্যু রিপোর্ট করুন
2. ফিচার সাজেশন দিন
3. Pull Request পাঠান

---

Made with ❤️ for Bengali coders!
