# Lis 🦊 Lang

A vibe-coded scripting language.

```lis
let me = {
    name: "lis",
    age: 1,
};

fn greet(person) {
    if person.age < 30 then {
        print "sup " ++ person.name;
    }
    else {
        print "hello " ++ person.name;
    }
}

greet(me);
```
