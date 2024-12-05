#!/usr/bin/env node

const fs = require('fs');

function get_data(filename) {
  let data = {};
  let x = 0, y = 0;

  chars = (fs.readFileSync(filename) + "").split('');

  for (let c of chars) {
    if (c != "\n") {
      data[`${x},${y}`] = c;
      x += 1;
    } else {
      y += 1;
      x = 0;
    }
  }

  return data;
}

const offsets = [
  [-1, -1],
  [ 0, -1],
  [ 1, -1],
  [-1,  0],
  [ 1,  0],
  [-1,  1],
  [ 0,  1],
  [ 1,  1]
];

function doOffset(offset, key) {
  let [x, y] = key.split(',').map(Number);
  x += offset[0];
  y += offset[1];
  return `${x},${y}`;
}

function helper1(data, key, offset, str, used) {
  console.log(`Hi Edward E, entering helper1, key:'${key}', offset:'${offset}, str:'${str}', used:`, used);
  if (! (key in data)) { return 0; }
  if (str == '') { return 1; }

  let head = str.charAt(0);
  console.log(`Hi Edward F, head:'${head}', data at key:'${data[key]}'`);
  if (data[key] != head) { return 0; }

  let tail = str.slice(1);
  console.log(`Hi Edward G, tail:'${tail}'`);

  return helper1(data, doOffset(offset, key), offset, tail, used.concat([key]));
}

function part1(filename) {
  console.log(`Hi Edward A, entering part1 with filename:'${filename}'`);
  let total = 0;
  let data = get_data(filename);

  for (let key in data) {
    if (data[key] != "X") { continue; }
    for (let i=0; i < offsets.length; i++) {
      let newKey = doOffset(offsets[i], key);
      console.log(`Hi Edward B, calling helper1 with key:'${key}', offset: '${offsets[i]}', newKey: '${newKey}'`);
      if (newKey in data) {
        total += helper1(data, key, offsets[i], "XMAS", []);
        console.log(`Hi Edward C, total:'${total}'`);
      }
    }
  }
  console.log(`Part 1 results from file '${filename}': ${total}`)
}


function find_neighbors(key) {
  let [x, y] = key.split(',').map(Number);
  return [
    `${x - 1},${y - 1}`,
    `${x},${y - 1}`,
    `${x + 1},${y - 1}`,
    `${x - 1},${y}`,
    `${x + 1},${y}`,
    `${x - 1},${y + 1}`,
    `${x},${y + 1}`,
    `${x + 1},${y + 1}`
  ];
}

function helper2(data, key, str, used) {
  console.log(`Hi Edward E, entering helper2, key:'${key}', str:'${str}'`);
  console.log(used);
  if (str == '') { return 1; }

  let head = str.charAt(0);
  console.log(`Hi Edward F, head:'${head}', data at key:'${data[key]}'`);
  if (data[key] != head) { return 0; }

  let tail = str.slice(1);
  console.log(`Hi Edward G, tail:'${tail}'`);
  let subtotal = 0;
  for (let neighbor of find_neighbors(key)) {
    console.log(`Hi Edward H, neighbor:'${neighbor}'`);
    if ((neighbor in data) && (! used.includes(neighbor))) {
      console.log(`Hi Edward I, recursing helper2, neighbor:'${neighbor}', tail:'${tail}'`);
      subtotal += helper2(data, neighbor, tail, used.concat([neighbor]));
      console.log(`Hi Edward J, subtotal: ${subtotal}`);
    }
  }

  return subtotal;
}

function part2(filename) {
  console.log(`Hi Edward A, entering part2 with filename:'${filename}'`);
  let total = 0;
  let data = get_data(filename);

  for (let key in data) {
    console.log(`Hi Edward B, calling helper2 with key:'${key}'`);
    total += helper2(data, key, "XMAS", [key]);
    console.log(`Hi Edward C, total:'${total}'`);
  }
  console.log(`Part 2 results from file '${filename}': ${total}`)
}

part1("test.txt");
// part1("input.txt");

