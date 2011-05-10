/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF med_aster_module supervis  DATE 10/05/2011   AUTEUR SELLENET N.SELLENET */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2011  EDF R&D              WWW.CODE-ASTER.ORG */
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
/* RESPONSABLE SELLENET N.SELLENET */
#include <Python.h>

#include "aster.h"
#include "aster_fort.h"
#include "aster_utils.h"
#include "med.h"

/* ---------------------------------------------------------------------- */
static PyObject * aster_nom_ch_med(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
   PyObject *listeChamps = (PyObject*)0 ;
   INTEGER numFichier = 0;
   char* nomFichierMed;
   
   if (!PyArg_ParseTuple(args, "s", &nomFichierMed)) return NULL;
   
   numFichier = MEDfileOpen(nomFichierMed,MED_ACC_RDONLY);
   if ( numFichier <= 0 ) return PyList_New(0);
   
   INTEGER nbChamps = MEDnField(numFichier);
   
   listeChamps = PyList_New(nbChamps);
   
   char* nomChamp = MakeBlankFStr(MED_NAME_SIZE);
   char* nomMaillage = MakeBlankFStr(MED_NAME_SIZE);
   med_bool mailLoc;
   med_field_type typeChamp;
   char* dtUnit = MakeBlankFStr(MED_NAME_SIZE);
   INTEGER nbCstp;
   INTEGER iChamp;
   for ( iChamp = 1; iChamp <= nbChamps; ++iChamp )
   {
      INTEGER nbComp = MEDfieldnComponent(numFichier,iChamp);
      
      char* nomsComp = MakeBlankFStr(nbComp*MED_SNAME_SIZE);
      char* unitesComp = MakeBlankFStr(nbComp*MED_SNAME_SIZE);
      
      MEDfieldInfo(numFichier,iChamp,nomChamp,nomMaillage,&mailLoc,&typeChamp,nomsComp,unitesComp,dtUnit,&nbCstp);
      
      PyList_SetItem(listeChamps,iChamp-1,PyString_FromString(nomChamp));
      
      FreeStr(nomsComp);
      FreeStr(unitesComp);
   }
   MEDfileClose(numFichier);
   
   FreeStr(nomChamp);
   FreeStr(nomMaillage);
   FreeStr(dtUnit);
   
   return listeChamps;
}

static PyMethodDef methods[] = {
   {"get_nom_champ_med", aster_nom_ch_med, METH_VARARGS},
   { NULL, NULL, 0, NULL }
};


PyMODINIT_FUNC initmed_fonctions(void)
{
   Py_InitModule("med_aster", methods);
}
