subroutine ef0409(nomte)
! aslint: disable=W0104
!     ----------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CALCUL DE EFGE_ELNO EN NON LINEAIRE
!     ------------------------------------------------------------------
    implicit none
!
#include "jeveux.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/ppgan2.h"
#include "asterfort/r8inir.h"
#include "asterfort/tecach.h"
#include "blas/dcopy.h"
! person_in_charge: sebastien.fayolle at edf.fr
!
    character(len=16) :: nomte
!
    integer :: nnos, ipoids, ivf, idfdx, jgano, jtab(7), icompo, ipg, npg
    integer :: ichn, icontm, nno, igeom, ndim, iret
!
!     ---> POUR DKT MATELEM = 3 * 6 DDL = 171 TERMES STOCKAGE SYME
!     ---> POUR DKQ MATELEM = 4 * 6 DDL = 300 TERMES STOCKAGE SYME
!
!     ---> POUR DKT EFFINT = 24
!     ---> POUR DKQ EFFINT = 32
    real(kind=8) :: effint(32)
!
! ---   RECUPERATION DES ADRESSES DANS ZR DES POIDS DES PG
!       DES FONCTIONS DE FORME DES VALEURS DES DERIVEES DES FONCTIONS
!       DE FORME ET DE LA MATRICE DE PASSAGE GAUSS -> NOEUDS
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfdx,jgano=jgano)
    call jevech('PEFFORR', 'E', ichn)
!
    call jevech('PGEOMER', 'L', igeom)
    call tecach('NNN', 'PCOMPOR', 'L', iret, iad=icompo)
    call tecach('OON', 'PCONTRR', 'L', iret, nval=7,&
                itab=jtab)
    call r8inir(32, 0.d0, effint, 1)
!
    do ipg = 1, npg
        icontm = jtab(1)+8*(ipg-1)
        call dcopy(8, zr(icontm), 1, effint(8*(ipg-1)+1), 1)
    end do
!
    call ppgan2(jgano, 1, 8, effint, zr(ichn))
!
end subroutine
