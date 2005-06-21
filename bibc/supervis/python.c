/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF config supervis  DATE 20/06/2005   AUTEUR BOITEAU O.BOITEAU */
/* RESPONSABLE                                 D6BHHJP J.P.LEFEBVRE   */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2001  EDF R&D              WWW.CODE-ASTER.ORG */
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
/* ALONG WITH THIS PROGRAM; IF NOT, WRITE TO : EDF R&D CODE_ASTER,    */
/*    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.     */
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
/* ALONG WITH THIS PROGRAM; IF NOT, WRITE TO : EDF R&D CODE_ASTER,    */
/*    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.     */
/* ================================================================== */
/* Minimal main program -- everything is loaded from the library */

#include "Python.h"
#ifdef MPI_FETI
#include "mpi.h"
#endif

extern DL_EXPORT(int) Py_Main();
extern void initaster();
extern void initaster_fonctions();

#ifdef MPI_FETI
void terminate(){
  printf("Fin interpreteur Python\n");
  MPI_Finalize();
}
#endif

int
main(argc, argv)
	int argc;
	char **argv;

{
        int me,ierr,rc,namelen,nbproc;
#ifdef MPI_FETI
	char processor_name[MPI_MAX_PROCESSOR_NAME];
        rc = MPI_Init(&argc,&argv);
	if (rc != MPI_SUCCESS) {
	     fprintf(stderr, "MPI Initialization failed: error code %d\n",rc);
	     abort();
	}  
        atexit(terminate);
        MPI_Comm_size(MPI_COMM_WORLD, &nbproc);
        MPI_Comm_rank(MPI_COMM_WORLD, &me);
        MPI_Get_processor_name(processor_name, &namelen);
        printf("Processeur/rang/nbproc= %s %d %d\n", processor_name,me,nbproc);
	
#endif	
	PyImport_AppendInittab("aster",initaster);

   /* Module définissant des opérations sur les objets fonction_sdaster */
	PyImport_AppendInittab("aster_fonctions",initaster_fonctions);

	ierr= Py_Main(argc, argv);
	return ierr;
}
