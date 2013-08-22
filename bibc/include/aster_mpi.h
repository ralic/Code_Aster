/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2012  EDF R&D              WWW.CODE-ASTER.ORG */
/*                                                                    */
/* THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR      */
/* MODIFY IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS     */
/* PUBLISHED BY THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE */
/* LICENSE, OR (AT YOUR OPTION) ANY LATER VERSION.                    */
/* THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,    */
/* BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF     */
/* MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU   */
/* GENERAL PUBLIC LICENSE FOR MORE DETAILS.                           */
/*                                                                    */
/* YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE  */
/* ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,      */
/*    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.     */
/* ================================================================== */
/* person_in_charge: mathieu.courtois at edf.fr */

#ifndef ASTER_MPI_H
#define ASTER_MPI_H

#include "aster.h"
#ifdef _USE_MPI
#   include "mpi.h"
#else
/*  for sequential */
#   define MPI_COMM_WORLD   0
#   define MPI_Fint         int
#   define MPI_Comm         int
#   define MPI_Comm_c2f(a)  a
#   define MPI_Comm_f2c(a)  a
#endif

/*
 *  Structure to store the communicator tree.
 * - id : the current communicator
 * - parent : its parent aster_comm_t communicator
 * - level: 0 for MPI_COMM_WORLD, +1 at each split
 * - childs: child communicators
 * - nbchild: number of childs
 * - name: for nicer print and debug
 */
#define MAX_CHILDS  10
#define NAME_LENGTH 16

typedef struct aster_comm_t aster_comm_t;

struct aster_comm_t {
    MPI_Comm id;
    aster_comm_t *parent;
    int level;
    aster_comm_t *childs[MAX_CHILDS];
    int nbchild;
    char name[NAME_LENGTH]; 
};

//#define UNITTEST
//#define COMM_DEBUG(c) printf("%-8s #%d (%d/@", (c).name, (int)MPI_Comm_c2f((c).id), (c).level); \
                      //if ((c).parent) { printf("%-8s", (c).parent->name); } \
                      //else { printf("        "); } \
                      //printf(")\n"); fflush(stdout);
#define COMM_DEBUG

/*
 *   PUBLIC FUNCTIONS
 *
 */

extern void aster_mpi_init(int, char **);

extern aster_comm_t* aster_get_comm_world();
extern aster_comm_t* aster_get_current_comm();
extern void aster_set_current_comm(aster_comm_t *);
extern void aster_get_mpi_info(aster_comm_t *, int *, int *);
extern aster_comm_t* aster_split_comm(aster_comm_t *, int, int, char *);
extern void aster_free_comm(aster_comm_t *);

extern void DEFSP(ASMPI_COMM, asmpi_comm,  char *, STRING_SIZE, MPI_Fint *);
extern void DEFPPPSP(ASMPI_SPLIT_COMM, asmpi_split_comm,
                     MPI_Fint *, MPI_Fint *, MPI_Fint *, char *, STRING_SIZE, MPI_Fint *);
extern void DEFPPP(ASMPI_INFO_WRAP, asmpi_info_wrap, MPI_Fint *, MPI_Fint *, MPI_Fint *);


#define CALL_ASABRT(a) CALLP(ASABRT,asabrt,a)
extern void DEFP( ASABRT, asabrt, _IN INTEGER * );
extern void terminate( void );

/*
 *   PRIVATE FUNCTIONS
 *
 */
extern void errhdlr_func(MPI_Comm *, int *, ... );
extern aster_comm_t* _search_id(aster_comm_t *, MPI_Comm *);
aster_comm_t* get_node_by_id(MPI_Comm *);
#ifdef UNITTEST
extern void _unittest_aster_mpi();
#endif

#endif
