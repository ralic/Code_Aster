subroutine vpddl(raide, masse, neq, nblagr, nbcine,&
                 neqact, dlagr, dbloq, ier)
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/pteddl.h"
#include "asterfort/typddl.h"
#include "asterfort/utmess.h"
!
    character(len=19) :: masse, raide
    integer :: neq, nblagr, nbcine, neqact, dlagr(neq), dbloq(neq), ier
!
!     ------------------------------------------------------------------
!     RENSEIGNEMENTS SUR LES DDL : LAGRANGE, BLOQUE, EXCLUS.
!     CONSTRUCTION DE TABLEAUX D'ENTIERS REPERANT LA POSITION DE CES DDL
!     ------------------------------------------------------------------
! IN  RAIDEUR : K  : NOM DE LA MATRICE DE "RAIDEUR"
! IN  MASSE   : K  : NOM DE LA MATRICE DE "MASSE"
! IN  NEQ     : IS : NPMBRE DE DDL
! OUT NBLAGR  : IS : NOMBRE DE DDL DE LAGRANGE
! OUT NBCINE  : IS : NOMBRE DE DDL BLOQUES PAR AFFE_CHAR_CINE
! OUT NEQACT  : IS : NOMBRE DE DDL ACTIFS
! OUT DLAGR   : IS : POSITION DES DDL DE LAGRANGE
! OUT DBLOQ   : IS : POSITION DES DDL BLOQUES PAR AFFE_CHAR_CINE
!
!
!
    integer ::  jccid, iercon, nbprno, ieq, mxddl, nba, nbb, nbl, nbliai, ifm, niv
    integer :: vali(4)
    character(len=14) :: nume
    parameter (mxddl=1)
    character(len=8) :: nomddl(mxddl)
!
    data nomddl/'LAGR'/
!
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!
    call jemarq()
!
!     ---RECUPERATION DU NIVEAU D'IMPRESSION---
    call infniv(ifm, niv)
!     -----------------------------------------
!
!     --- CALCUL DU NOMBRE DE LAGRANGES ---
!     -------------------------------------
!
!       --- RECUPERATION DU NOM DE LA NUMEROTATION ASSOCIEE AUX MATRICES
    call dismoi('NOM_NUME_DDL', raide, 'MATR_ASSE', repk=nume)
!
!       --- RECUPERATION DES POSITIONS DES DDL LAGRANGE : DLAGR
    call pteddl('NUME_DDL', nume, mxddl, nomddl, neq,&
                dlagr)
!
!       --- CALCUL DU NOMBRE DE 'LAGRANGE': NBLAGR
    nblagr = 0
    do 10 ieq = 1, neq
        nblagr = nblagr + dlagr(ieq)
 10 end do
!
!       --- INVERSION : DLAGR = 0 SI LAGRANGE ET 1 SINON
    do 20 ieq = 1, neq
        dlagr(ieq) = abs(dlagr(ieq)-1)
 20 end do
!
!     --- DETECTION DES DDL BLOQUES PAR AFFE_CHAR_CINE ---
!     ----------------------------------------------------
!
    call typddl('ACLA', nume, neq, dbloq, nba,&
                nbb, nbl, nbliai)
!
!       --- MISE A JOUR DE DBLOQ QUI VAUT 0 POUR TOUS LES DDL BLOQUES
    call jeexin(masse//'.CCID', iercon)
    nbcine = 0
    if (iercon .ne. 0) then
        call jeveuo(masse//'.CCID', 'E', jccid)
        do 30 ieq = 1, neq
            dbloq(ieq) = dbloq(ieq)*abs(zi(jccid+ieq-1)-1)
 30     continue
!
!       --- CALCUL DU NOMBRE DE DDL BLOQUE PAR CETTE METHODE : NCINE ---
        do 40 ieq = 1, neq
            nbcine = nbcine + zi(jccid+ieq-1)
 40     continue
    endif
!
!     --- SI NUMEROTATION GENERALISEE : PAS DE DDLS BLOQUES ---
!     ---------------------------------------------------------
    call jenonu(jexnom(nume//'.NUME.LILI', '&SOUSSTR'), nbprno)
    if (nbprno .ne. 0) then
        do 50 ieq = 1, neq
            dbloq(ieq) = 1
 50     continue
    endif
!
!     ----------------- CALCUL DU NOMBRE DE DDL ACTIFS -----------------
    neqact = neq - 3* (nblagr/2) - nbcine
    if (neqact .le. 0) then
        call utmess('F', 'ALGELINE3_63')
    endif
!
!    -----IMPRESSION DES DDL -----
!
    if (niv .ge. 1) then
        write (ifm,9000)
        vali(1) = neq
        vali(2) = nblagr
        if (nbcine .eq. 0) then
            vali(3) = neqact
            call utmess('I', 'ALGELINE7_17', ni=3, vali=vali)
        else
            vali(3) = nbcine
            vali(4) = neqact
            call utmess('I', 'ALGELINE7_18', ni=4, vali=vali)
        endif
        write (ifm,9010)
    endif
!     -----------------------------------------------------------------
!     -----------------------------------------------------------------
!
    ier = 0
    call jedema()
!
    9000 format (//,72 ('-'))
    9010 format (72 ('-'))
!
end subroutine
