/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF DATE UTILITAI  DATE 22/03/2011   AUTEUR COURTOIS M.COURTOIS */
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
#include <time.h>
#include "aster.h"

void DEFP(DATE, date, INTEGER itab[6])
{
    time_t ltime;
    struct tm *today;

    time( &ltime );
    today = localtime(&ltime);

    itab[0] = today->tm_year;
    itab[1] = today->tm_mon + 1;
    itab[2] = today->tm_mday;

    itab[3] = today->tm_hour ;
    itab[4] = today->tm_min  ;
    itab[5] = today->tm_sec  ;

}
