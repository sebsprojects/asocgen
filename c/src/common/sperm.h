#ifndef SPERM
#define SPERM

#include "common.h"

struct Array_uint16;


/*
  Fills the array with 0xffff and fills to id = (0 1 2 3 ... n - 1 0xffff ...)
  before (excluding) index n
 */
void initBinom(struct Array_uint16 *array, uint16_t n);


/*
 * Fills the whole array with id = (0 1 2 3 ... array->size - 1)
 */
void initPerm(struct Array_uint16 *array);


/*
  Shifts the array by one step (if possible) according to the following scheme:
    1. [ a b ... x [x + >= 2] ... ]      -> [ 0 1 ... [x + 1] [x + >= 2] ... ]
    2. [ a b ... x [x + 1] 0xffff ... ]  -> [ 0 1 ... x [x + 2 < max] ...]
    3. [ 0 1 ... [max - 2] [max - 1] 0xffff ...]   -> no action
  For example n = 3, max = 5
    ( 0 1 2 0xffff ...) -> (case 2)
    ( 0 1 3 0xffff ...) ->
    ( 0 2 3 0xffff ...) ->
    ( 1 2 3 0xffff ...) -> (case 2)
    ( 0 1 4 0xffff ...) ->
    ( 0 2 4 0xffff ...) ->
    ( 1 2 4 0xffff ...) ->
    ( 0 3 4 0xffff ...) ->
    ( 1 3 4 0xffff ...) ->
    ( 2 3 4 0xffff ...) -> (case 3)
  Input:
    array -  array of the form a[0] < a[1] < ... < a[n-1], 0xffff, 0xffff, ...
    max   -  1 + maximal element to occur in array (ie. array[<n] < max)
  Return:
    1  -  if shift was possible (case 1, 2)
    0  -  if shift was impossible (case 3)
*/
bool shiftBinom(struct Array_uint16 *array, uint16_t max);


/*
  Shifts the array by one step towards the "numerical" maximum value if
  you take the elements of the array as digits of a number
  For Example:
    ( 0 1 2 3 ) ->
    ( 0 1 3 2 ) ->
    ( 0 2 1 3 ) ->
    ( 0 2 3 1 ) ->
    ( 1 0 2 3 ) ->
    ( 1 0 3 2 ) ->
    ...
 */
bool shiftPerm(struct Array_uint16 *array);

#endif
