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

/* This module defines functions:
 * - to manage the MPI communicators
 * - to properly interrupt a MPI execution.
 * 
 * The communicators are managed in C, Fortran calls these functions.
 * But all the communications are initiated from the Fortran subroutines.
 * Example: Fortran calls asmpi_comm() here,
 * but aster_set_mpi_barrier() calls asmpi_barrier() from asmpi_barrier.F90
 * 
 * Communicators are store in fortran as Code_Aster mpi_int (== MPI_Fint).
 * They are converted to MPI_Comm with MPI_Comm_f2c((MPI_Fint)fortran_comm)
 * 
 * Naming convention:
 *      aster_mpi_xxx : C functions and global variable
 *      asmpi_xxx : Fortran functions
 */
#define USE_ASSERT

#include "aster.h"
#include "aster_mpi.h"
#include "aster_utils.h"
#include "aster_fort.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Global object that store the entire tree */
static aster_comm_t aster_mpi_world;

/* and a pointer to the current node */
static aster_comm_t *aster_mpi_current = NULL;

#ifdef _USE_MPI
static MPI_Errhandler errhdlr;
#endif

/*
 *   PUBLIC FUNCTIONS
 * 
 */
void aster_mpi_init(int argc, char **argv)
{
    /* MPI initialization */
#ifdef _USE_MPI

    AS_ASSERT(MPI_Init(&argc, &argv) == MPI_SUCCESS);
    AS_ASSERT(atexit(terminate) == 0);
    /* set the error handler */
    AS_ASSERT(MPI_Comm_create_errhandler(errhdlr_func, &errhdlr) == MPI_SUCCESS);
    AS_ASSERT(MPI_Comm_set_errhandler(MPI_COMM_WORLD, errhdlr) == MPI_SUCCESS);
#endif
    aster_mpi_world.id = MPI_COMM_WORLD;
    aster_mpi_world.parent = NULL;
    aster_mpi_world.level = 0;
    strncpy(aster_mpi_world.name, "WORLD", NAME_LENGTH);
    aster_mpi_current = &aster_mpi_world;
#ifdef UNITTEST
    _unittest_aster_mpi();
#endif
    return;
}

/* API that works on aster_comm_t */
aster_comm_t* aster_get_comm_world() {
    /* Return the original "MPI_COMM_WORLD" node */
    return &aster_mpi_world;
}

aster_comm_t* aster_get_current_comm() {
    /* Return the current node */
    return aster_mpi_current;
}

void aster_set_current_comm(aster_comm_t *node) {
    /* Assign the current communicator */
    aster_mpi_current = node;
}

void aster_get_mpi_info(aster_comm_t *node, int *rank, int *size) {
    /* Return the rank of the process in `node` and its size */
    *rank = 0;
    *size = 1;
    COMM_DEBUG(*node);
#ifdef _USE_MPI
    MPI_Comm_rank( node->id, rank );
    MPI_Comm_size( node->id, size );
#endif
    return;
}

aster_comm_t* aster_split_comm(aster_comm_t *parent, int color, int key, char *name) {
    /* Split the given communicator using color/key args,
     * return the sub-communicator */
    aster_comm_t *new;
#ifdef _USE_MPI
    MPI_Errhandler hdlr;
    int ierr;

    new = (aster_comm_t *)malloc(sizeof(aster_comm_t));
    ierr = MPI_Comm_split(parent->id, color, key, &(new->id));
    AS_ASSERT(ierr == MPI_SUCCESS);
    /* the parent has a new child */
    AS_ASSERT(parent->nbchild < MAX_CHILDS);
    parent->childs[parent->nbchild] = new;
    parent->nbchild++;
    /* fill the new node */
    new->parent = parent;
    new->level = parent->level + 1;
    new->nbchild = 0;
    strncpy(new->name, name, NAME_LENGTH);
    /* transfert the error handler - maybe already done by MPI_Comm_split */
    AS_ASSERT(MPI_Comm_get_errhandler(parent->id, &hdlr) == MPI_SUCCESS);
    AS_ASSERT(MPI_Comm_set_errhandler(new->id, hdlr) == MPI_SUCCESS);
    COMM_DEBUG(*new);
#else
    new = NULL;
#endif
    return new;
}

void aster_free_comm(aster_comm_t *node) {
    /* delete this node */
#ifdef _USE_MPI
    aster_comm_t *parent;
    int i=0;
    int nb, j, ierr;
    
    AS_ASSERT(node->nbchild == 0);
    /* remove node from its parent childs list*/
    parent = node->parent;
    nb = parent->nbchild;
    while ( i < nb && parent->childs[i] != node ) {
        i++;
    }
    AS_ASSERT( i < nb );
    for(j=i+1; j < nb; j++) {
        parent->childs[j-1] = parent->childs[j];
    }
    parent->childs[nb-1] = NULL;
    parent->nbchild = nb - 1;
    /* delete the MPI_Comm */
    ierr = MPI_Comm_free(&(node->id));
    AS_ASSERT(ierr == MPI_SUCCESS);
    free(node);
#endif
    return;
}

void aster_set_mpi_barrier(aster_comm_t *node) {
    /* Set a MPI barrier */
#ifdef _USE_MPI
    MPI_Fint comm;
    comm = MPI_Comm_c2f(node->id);
    CALL_ASMPI_BARRIER(&comm);
#endif
    return;
}

/* Access functions */
aster_comm_t* _search_id(aster_comm_t *node, MPI_Comm *id) {
    /* Search for 'id' in 'node' and its childs
     * Return NULL if not found.
     */
    aster_comm_t *found;
    int i;

    if (node->id == *id) {
        return node;
    } else {
        for(i=0; i < node->nbchild; i++) {
            found = _search_id(node->childs[i], id);
            if (found) {
                return found;
            }
        }
    }
    return NULL;
}

aster_comm_t* get_node_by_id(MPI_Comm *id) {
    /* Return the node that has the given 'id' */
    aster_comm_t *node;
    
    node = _search_id(&aster_mpi_world, id);
    AS_ASSERT( node );
    return node;
}

/*
 *  Fortran interfaces - wrappers of the C functions
 *
 */
void DEFSP(ASMPI_COMM, asmpi_comm,_IN char *action, STRING_SIZE lact,
                                _INOUT MPI_Fint *comm) {
    /* Wrapper around:
     *  aster_get_comm_world:   action = 'GET_WORLD', comm is OUT
     *  aster_get_current_comm: action = 'GET',       comm is OUT
     *  aster_set_current_comm: action = 'SET',       comm is IN
     *  aster_free_comm:        action = 'FREE',      comm is IN
     */
    MPI_Comm mpicom;
    aster_comm_t *node;
    char *act;
    
    act = MakeCStrFromFStr(action, lact);
    if (strcmp(act, "GET_WORLD") == 0) {
        *comm = MPI_Comm_c2f(aster_get_comm_world()->id);
    } else if  (strcmp(act, "GET") == 0) {
        *comm = MPI_Comm_c2f(aster_get_current_comm()->id);
    } else if  (strcmp(act, "SET") == 0) {
        mpicom = MPI_Comm_f2c( *comm );
        node = get_node_by_id(&mpicom);
        aster_set_current_comm(node);
    } else if  (strcmp(act, "FREE") == 0) {
        mpicom = MPI_Comm_f2c( *comm );
        node = get_node_by_id(&mpicom);
        aster_free_comm(node);
    } else {
        AS_ASSERT(0);
    }
    FreeStr(act);
    return;
}

void DEFPPPSP(ASMPI_SPLIT_COMM, asmpi_split_comm,
                    _IN MPI_Fint *parent,
                    _IN MPI_Fint *color, MPI_Fint *key, 
                    _IN char *name, STRING_SIZE lname,
                   _OUT MPI_Fint *newcomm) {
    /* Wrapper around aster_split_comm */
    MPI_Comm mpicom;
    aster_comm_t *new;
    char *newname;
    
    newname = MakeCStrFromFStr(name, lname);
    mpicom = MPI_Comm_f2c( *parent );
    new = aster_split_comm(get_node_by_id(&mpicom), (int)*color, (int)*key, newname);
    *newcomm = MPI_Comm_c2f(new->id);
    FreeStr(newname);
    return;
}

void DEFPPP(ASMPI_INFO_WRAP, asmpi_info_wrap, MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *size) {
    /* Wrapper around aster_get_mpi_info
     * Called by the fortran subroutine asmpi_info where all arguments are optional.
     */
    MPI_Comm mpicom;
    aster_comm_t *node;
    int irank=0, isize=1;
    
    AS_ASSERT(sizeof(MPI_Fint) == 4);
    
    mpicom = MPI_Comm_f2c( *comm );
    node = get_node_by_id(&mpicom);
    COMM_DEBUG(*node);
    aster_get_mpi_info(node, &irank, &isize);
    *rank = (MPI_Fint)irank;
    *size = (MPI_Fint)isize;
    return;
}

/*
 * Define a dedicated function to abort a Code_Aster execution.
 */
int gErrFlg = 0;

void DEFP( ASABRT, asabrt, _IN INTEGER *iret )
{
    /* Function to interrupt the execution.
     * - In a sequential version, it just calls abort().
     * - In a MPI execution, it set a global flag and calls `MPI_Abort`.
     * 
     * The usage of atexit seems required in a Python interpreter
     * certainly because `sys.exit()` probably calls the `exit` system
     * function (so we can't add a `MPI_Finalize` call before exiting).
     * But if `MPI_Finalize` is executed after a `MPI_Abort` call, all
     * the processes are not interrupted.
     * That's why a global flag is used to by-pass `MPI_Finalize` in
     * case of error.
     * 
     * to test MPI_Abort : http://www.netlib.org/blacs/blacs_errata.html
     */
    gErrFlg = 1;
#ifdef _USE_MPI
    MPI_Abort( MPI_COMM_WORLD, (int)(*iret) );
#else
    CALL_ABORTF();
#endif
    return;
}

void terminate( void )
{
    /* Function registered using atexit() in main.
     */
    printf("End of the Code_Aster execution");
#ifdef _USE_MPI
    if ( gErrFlg == 0 ) {
        printf(" - MPI exits normally\n");
        MPI_Errhandler_free(&errhdlr);
        MPI_Finalize();
    } else {
        printf(" - MPI exits with errors\n");
    }
#endif
    printf("\n");
    return;
}

/*
 *   PRIVATE FUNCTIONS
 * 
 */
#ifdef _USE_MPI
void errhdlr_func(MPI_Comm *comm, int *err, ... ) {
    /* Error handler for calls to MPI functions */
    char errstr[MPI_MAX_ERROR_STRING];
    int len;

    AS_ASSERT(*err != MPI_SUCCESS)
    MPI_Error_string(*err, errstr, &len);
    printf("\n<F> MPI Error code %d:\n    %s\n\n", *err, errstr);
    fflush(stdout);
    CALL_U2MESS("F", "APPELMPI_5");
    return;
}
#endif

/* UNITTEST */
#ifdef UNITTEST
void _unittest_aster_mpi() {
    /* unittest of the functions on aster_comm_t tree */
    int size, rank, npband, npsolv;
    int color;
    aster_comm_t *node, *world, *scband, *sccross, *scsolv;

    COMM_DEBUG(aster_mpi_world);
    aster_get_mpi_info(&aster_mpi_world, &rank, &size);

    world = aster_get_comm_world();
    node = aster_get_current_comm();
    AS_ASSERT(world == node);

    npband = size / 2;
    npsolv = size / 4;
    if (npsolv < 1) {
        printf("this test requires at least 4 procs, 8 to be relevant\n");
        return;
    }
    color = rank < npband;
    
    fprintf(stderr, "band color : %d\n", color);
    scband = aster_split_comm(world, color, rank, "band");
    aster_set_current_comm(scband);
    COMM_DEBUG(*aster_mpi_current);
    
    color = rank % npband == 0;
    fprintf(stderr, "cross color : %d\n", color);
    sccross = aster_split_comm(world, color, rank, "cross");
    COMM_DEBUG(*sccross);

    color = rank % npsolv == 0;
    fprintf(stderr, "solv color : %d\n", color);
    scsolv = aster_split_comm(aster_get_current_comm(), color, rank, "solver");


    aster_get_mpi_info(world, &rank, &size);
    fprintf(stderr, "%-8s: rank=%d  size=%d\n", world->name, rank, size);
    
    aster_get_mpi_info(scband, &rank, &size);
    fprintf(stderr, "%-8s: rank=%d  size=%d\n", scband->name, rank, size);
    
    aster_get_mpi_info(sccross, &rank, &size);
    fprintf(stderr, "%-8s: rank=%d  size=%d\n", sccross->name, rank, size);
    
    aster_get_mpi_info(scsolv, &rank, &size);
    fprintf(stderr, "%-8s: rank=%d  size=%d\n", scsolv->name, rank, size);
    
    aster_set_current_comm(world);
    return;
}
#endif
