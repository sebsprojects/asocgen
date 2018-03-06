#include "smap.h"

#include <math.h>

Map_uint16 *mapui16_alloc(uint32_t size, bool indexed) {
  Map_uint16 *map = malloc(sizeof(Map_uint16));
  map->indexed = indexed;
  map->domain = aui16_alloc(size);
  map->codomain = aui16_alloc(size);
  return map;
}

Map_uint16 *mapui16_alloc_ref(uint32_t size, bool indexed,
                              Array_uint16 *domain,
                              Array_uint16 *codomain) {
  Map_uint16 *map = malloc(sizeof(Map_uint16));
  map->indexed = indexed;
  map->domain = domain;
  map->codomain = codomain;
  return map;
}

void mapui16_free(Map_uint16 *map) {
  aui16_free(map->codomain);
  aui16_free(map->domain);
  free(map);
}

void mapui16_free_ref(Map_uint16 *map) {
  free(map);
}

// -------------------------------------------------------------------------


void mapui16_mapArray(Map_uint16 *map, Array_uint16 *from, Array_uint16 *to) {
  uint32_t i;
  for(i = 0; i < from->size; i++) {
    *aui16_at(to, i) = mapui16_mapEle(map, *aui16_at(from, i));
  }
}

void mapui16_toId(Map_uint16 *map) {
  map->indexed = 1;
  uint16_t i;
  for(i = 0; i < map->domain->size; i++) {
    *aui16_at(map->domain, i) = i;
    *aui16_at(map->codomain, i) = i;
  }
}

bool mapui16_isValid(Map_uint16 *map) {
  uint32_t i, j;
  uint32_t n = map->domain->size;
  if(n != map->codomain->size) {
    return 0;
  }
  for(i = 0; i < n; i++) {
    for(j = 0; j < i; j++) {
      if(*aui16_at(map->domain, i) == *aui16_at(map->domain, j)) {
        return 0;
      }
    }
    if(*aui16_at(map->codomain, i) == 0xffff ||
       *aui16_at(map->domain, i) == 0xffff) {
      return 0;
    }
  }
  return 1;
}

bool mapui16_areEqual(Map_uint16 *f, Map_uint16 *g) {
  if(f->domain->size != g->domain->size) {
    return 0;
  }
  if(f->indexed && g->indexed) {
    return aui16_areEqualArrays(f->codomain, g->codomain);
  }
  uint32_t i, ind;
  for(i = 0; i < f->domain->size; i++) {
    ind = aui16_indexOf(g->domain, *aui16_at(f->domain, i));
    if(ind == 0xffff) { // ele does not exist in domain of g
      return 0;
    }
    if(mapui16_mapInd(f, i) != mapui16_mapInd(g, ind)) { // f(ele) != g(ele)
      return 0;
    }
  }
  return 1;
}

bool mapui16_areComposable(Map_uint16 *f, Map_uint16 *g) {
  uint32_t i;
  for(i = 0; i < g->codomain->size; i++) {
    if(aui16_indexOf(f->domain, *aui16_at(g->codomain, i)) == 0xffff) {
      return 0;
    }
  }
  return 1;
}

bool mapui16_isSurjectiveIn(Map_uint16 *map, Array_uint16 *set) {
  return aui16_isSubset(set, map->codomain);
}

bool mapui16_isInjective(Map_uint16 *map) {
  return !aui16_hasDuplicates(map->codomain);
}

void mapui16_comp_noalloc(Map_uint16 *f, Map_uint16 *g, Map_uint16 *comp,
                          bool setDomain) {
  uint32_t i;
  for(i = 0; i < g->domain->size; i++) {
    if(setDomain) {
      *aui16_at(comp->domain, i) = *aui16_at(g->domain, i);
    }
    *aui16_at(comp->codomain, i) = mapui16_mapEle(f, mapui16_mapInd(g, i));
  }
}

Map_uint16 *mapui16_comp_alloc(Map_uint16 *f, Map_uint16 *g) {
  Map_uint16 *comp = mapui16_alloc(g->domain->size, g->indexed);
  mapui16_comp_noalloc(f, g, comp, 0);
  return comp;
}

bool mapui16_hasNotfixedPoints(Map_uint16 *map) {
  uint32_t i;
  for(i = 0; i < map->domain->size; i++) {
    if(*aui16_at(map->domain, i) != mapui16_mapInd(map, i)) {
      return 1;
    }
  }
  return 0;
}

uint16_t mapui16_getMaximalNotfixedImage(Map_uint16 *map) {
#ifdef BOUNDS_CHECK
  if(!mapui16_hasNotfixedPoints(map)) {
    printError("mapui16: has no notfixed point!");
    exit(1);
  }
#endif
  uint32_t i;
  uint16_t max = 0;
  uint16_t ele;
  for(i = 0; i < map->domain->size; i++) {
    ele = *aui16_at(map->domain, i);
    if(mapui16_mapInd(map, i) != ele) {
      max = umax(mapui16_mapInd(map, i), max);
    }
  }
  return max;
}

// -------------------------------------------------------------------------

void mapui16_sprintLine(Map_uint16 *map, char *pstring, uint32_t indFrom,
                        uint32_t indTo, uint32_t indent, uint32_t digitPad) {
  aui16_sprintLine(map->domain, pstring, indFrom, indTo, indent, digitPad);
  sprintf(pstring + strlen(pstring), "\n");
  aui16_sprintLine(map->codomain, pstring, indFrom, indTo, indent, digitPad);
}

// mostly c/p sarray aui16_sprintToNum
void mapui16_sprintToNum(Map_uint16 *map, char *pstring,
                         uint32_t elePerLine, uint32_t indent) {
  if(map->domain->size == 0) {
    sprintf(pstring + strlen(pstring), "<   ]");
    return;
  }
  Array_uint16 *dom = map->domain;
  Array_uint16 *cod = map->codomain;
  uint32_t max = umax(aui16_getMax(dom), aui16_getMax(cod));
  uint32_t digitPad = max == 0 ? 1 : floor(log10(max)) + 1;
  uint32_t n = dom->size;
  uint32_t numLines = ceil((float) n / elePerLine);
  uint32_t eleInLastLine = n % elePerLine;
  uint32_t entrySize = 2 + digitPad;
  // first line
  padString(pstring + strlen(pstring), indent); // manual indent b/c "<"
  sprintf(pstring + strlen(pstring), "< ");
  aui16_sprintLine(dom, pstring, 0, umin(dom->size, elePerLine), 0, digitPad);
  if(numLines == 1) {
    sprintf(pstring + strlen(pstring), " >\n");
  } else {
    sprintf(pstring + strlen(pstring), ",\n");
  }
  padString(pstring + strlen(pstring), indent);
  sprintf(pstring + strlen(pstring), "< ");
  aui16_sprintLine(cod, pstring, 0, umin(cod->size, elePerLine), 0, digitPad);
  if(numLines == 1) {
    sprintf(pstring + strlen(pstring), " >");
  }
  uint32_t i;
  // 1 until before last line
  for(i = 1; i < numLines - 1; i++) {
    sprintf(pstring + strlen(pstring), ",\n\n  ");
    aui16_sprintLine(dom, pstring, i * elePerLine, (i + 1) * elePerLine,
                     indent, digitPad);
    sprintf(pstring + strlen(pstring), ",\n  ");
    aui16_sprintLine(cod, pstring, i * elePerLine, (i + 1) * elePerLine,
                     indent, digitPad);
  }
  // last line
  if(numLines > 1) {
    sprintf(pstring + strlen(pstring), ",\n\n  ");
    aui16_sprintLine(dom, pstring, (numLines - 1) * elePerLine,
                     umin(dom->size, numLines * elePerLine),
                     indent, digitPad);
    if(eleInLastLine > 0) {
      padString(pstring, (elePerLine - eleInLastLine) * entrySize);
    }
    sprintf(pstring + strlen(pstring), " >\n  ");
    aui16_sprintLine(cod, pstring, (numLines - 1) * elePerLine,
                     umin(cod->size, numLines * elePerLine),
                     indent, digitPad);
    if(eleInLastLine > 0) {
      padString(pstring, (elePerLine - eleInLastLine) * entrySize);
    }
    sprintf(pstring + strlen(pstring), " >");
  }
}

void mapui16_sprintToWidth(Map_uint16 *map, char *pstring,
                           uint32_t width, uint32_t indent) {
  // inefficient b/c computes again in sprintToNum but whatever
  uint32_t max = umax(aui16_getMax(map->domain), aui16_getMax(map->codomain));
  uint32_t digitPad = max == 0 ? 1 : floor(log10(max)) + 1;
  uint32_t entrySize = 2 + digitPad;
  uint32_t additionalSpace = 2 + 2;
  if(width < indent + additionalSpace + entrySize) { // not enough line wid
    return;
  }
  uint32_t elePerLine = (width - indent - additionalSpace) / entrySize;
  mapui16_sprintToNum(map, pstring, elePerLine, indent);
}

void mapui16_sprintDefault(Map_uint16 *map, char *pstring) {
  mapui16_sprintToWidth(map, pstring, 80, 0);
}

void mapui16_printToNum(Map_uint16 *map,uint32_t elePerLine,uint32_t indent) {
  uint32_t n = map->domain->size;
  char *pstring = malloc(128 + n * 16 * 2); // pretty safe guess I tk
  pstring[0] = '\0';
  mapui16_sprintToNum(map, pstring,  elePerLine, indent);
  printf("%s\n", pstring);
  free(pstring);
}

void mapui16_printToWidth(Map_uint16 *map, uint32_t width, uint32_t indent) {
  uint32_t n = map->domain->size;
  char *pstring = malloc(128 + n * 16 * 2); // pretty safe guess I tk
  pstring[0] = '\0';
  mapui16_sprintToWidth(map, pstring, width, indent);
  printf("%s\n", pstring);
  free(pstring);
}

void mapui16_printDefault(Map_uint16 *map) {
  uint32_t n = map->domain->size;
  char *pstring = malloc(128 + n * 16 * 2); // pretty safe guess I tk
  pstring[0] = '\0';
  mapui16_sprintDefault(map, pstring);
  printf("%s\n", pstring);
  free(pstring);
}
