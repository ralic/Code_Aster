subroutine inmain(nommat, neq, nozero)
!
    implicit none
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!    M. CORUS     DATE 11/03/10
!-----------------------------------------------------------------------
!  BUT:      < INITIALISER LES MATRICES D'INTERFACE >
!C
!-----------------------------------------------------------------------
!  NOMMAT    /I/ : NOM DE LA MATRICE
!  NEQ       /I/ : NOMBRE D'EQUATIONS
!  NOZERO   /O/ : NOMBRE DE TERMES NONS NULS
!
!     ------------------------------------------------------------------
!
!-- VARIABLES EN ENTREES / SORTIE
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    integer :: neq, nozero
    character(len=19) :: nommat
!
!-- VARIABLES DE LA ROUTINE
    integer :: ibid, jrefa, i1, j1, iret
!
!-----------C
!--       --C
!-- DEBUT --C
!--       --C
!-----------C
!
    call jemarq()
!
!-- CREATION DU .REFA
    call wkvect(nommat//'.REFA', 'V V K24', 20, jrefa)
    zk24(jrefa-1+11)='MPI_COMPLET'
    zk24(jrefa-1+1)=' '
    zk24(jrefa-1+2)='&&NUME91'
    zk24(jrefa-1+8) = 'ASSE'
    zk24(jrefa-1+9) = 'MS'
    zk24(jrefa-1+10) = 'GENE'
    zk24(jrefa-1+7) = '&&NUME91      .SOLV'
!
!
!-- CREATION DU .LIME
    call wkvect(nommat//'.LIME', 'V V K24', 1, ibid)
    zk24(ibid)='&&MODL91'
!
!-- CREATION DU .CONL
    call wkvect(nommat//'.CONL', 'V V R', neq, j1)
    do 10 i1 = 1, neq
        zr(j1+i1-1)=1.d0
10  end do
!
!-- .VALM NE DOIT PAS EXISTER :
    call jeexin(nommat//'.VALM', iret)
    ASSERT(iret.eq.0)
!
!-- ALLOCATION DES MATRICES D'INTERFACE
    call jecrec(nommat//'.VALM', 'V V R', 'NU', 'DISPERSE', 'CONSTANT',&
                1)
!
    call jecroc(jexnum(nommat//'.VALM', 1))
!
    call jeecra(jexnum(nommat//'.VALM', 1), 'LONMAX', nozero)
!
!---------C
!--     --C
!-- FIN --C
!--     --C
!---------C
    call jedema()
end subroutine
