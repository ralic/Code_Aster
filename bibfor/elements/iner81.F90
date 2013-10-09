subroutine iner81(nomres, classe, basmod, nommat)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!***********************************************************************
!
!  BUT : CALCUL DES FORCES D'INERTIES SUR BASE MODALE
!
!-----------------------------------------------------------------------
!
! NOMRES /I/ : NOM K19 DE LA MATRICE CARREE RESULTAT
! CLASSE /I/ : CLASSE DE LA BASE JEVEUX DE L'OBJET RESULTAT
! BASMOD /I/ : NOM UT DE LA BASE MODALE DE PROJECTION
! NOMMAT /I/ : NOM K8 DE LA MATRICE A PROJETER
!
!
!
!
#include "jeveux.h"
#include "asterfort/copmod.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtdscr.h"
#include "asterfort/mtexis.h"
#include "asterfort/pteddl.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/zerlag.h"
#include "blas/dcopy.h"
#include "blas/ddot.h"
!-----------------------------------------------------------------------
    integer :: i, ia, iad, idbase, iddeeq, ieq
    integer :: ier, if, ldref, ldres, lmat, ltvec1
    integer :: ltvec2, ltvec3, mxddl, nbdef, neq
!-----------------------------------------------------------------------
    parameter     (mxddl=6)
    character(len=8) :: nomddl(mxddl)
    character(len=1) :: classe
    character(len=6) :: pgc
    character(len=19) :: nommat
    character(len=8) :: basmod
    character(len=14) :: num
    character(len=24) :: nomres
    character(len=24) :: valk
    complex(kind=8) :: cbid
!
!-----------------------------------------------------------------------
    data pgc /'INER81'/
    data nomddl/'DX      ','DY      ','DZ      ',&
     &            'DRX     ','DRY     ','DRZ     '/
!-----------------------------------------------------------------------
!
! --- CREATION DU .REFE
!
    call jemarq()
    call wkvect(nomres(1:18)//'_REFE', 'G V K24', 2, ldref)
    zk24(ldref)=basmod
    zk24(ldref+1)=nommat
!
! --- NOMBRE TOTAL DE MODES ET DEFORMEES
!
    call dismoi('NB_MODES_TOT', basmod, 'RESULTAT', repi=nbdef)
!
!
! --- ALLOCATION DE LA MATRICE RESULTAT
!
    call wkvect(nomres(1:18)//'_VALE', classe//' V R', 3*nbdef, ldres)
!
! --- CONTROLE D'EXISTENCE DE LA MATRICE
!
    call mtexis(nommat(1:8), ier)
    if (ier .eq. 0) then
        valk = nommat(1:8)
        call utmess('F', 'ALGORITH12_39', sk=valk)
    endif
!
! --- ALLOCATION DESCRIPTEUR DE LA MATRICE
!
    call mtdscr(nommat(1:8))
    call jeveuo(nommat(1:19)//'.&INT', 'E', lmat)
!
! --- RECUPERATION NUMEROTATION ET NB EQUATIONS
!
    call dismoi('NB_EQUA', nommat(1:8), 'MATR_ASSE', repi=neq)
    call dismoi('NOM_NUME_DDL', nommat(1:8), 'MATR_ASSE', repk=num)
!
! --- ALLOCATION VECTEURS DE TRAVAIL
!
    call wkvect('&&'//pgc//'.VECT1', 'V V R', neq, ltvec1)
    call wkvect('&&'//pgc//'.VECT2', 'V V R', neq, ltvec2)
    call wkvect('&&'//pgc//'.VECT3', 'V V I', mxddl*neq, ltvec3)
    call pteddl('NUME_DDL', num, mxddl, nomddl, neq,&
                zi(ltvec3))
!
    call jeveuo(num//'.NUME.DEEQ', 'L', iddeeq)
    call wkvect('&&'//pgc//'.BASEMO', 'V V R', nbdef*neq, idbase)
    call copmod(basmod, 'DEPL', neq, num, nbdef,&
                'R', zr(idbase), [cbid])
!
! --- CALCUL DES FORCES D'INERTIES
!
    do 30 if = 1, 3
!
!     --- MODE RIGIDE EN DX , DY , DZ
!
        ia = (if-1)*neq
        do 10 ieq = 0, neq-1
            zr(ltvec1+ieq) = zi(ltvec3+ia+ieq)
 10     continue
!
!     --- MULTIPLICATION DU MODE RIGIDE PAR LA MATRICE MASSE
!
        call mrmult('ZERO', lmat, zr(ltvec1), zr(ltvec2), 1,&
                    .true.)
!
!     --- PROJECTION SUR LES MODES PROPRES ET LES DEFORMEES NON MODALES
!
        iad = (if-1)*nbdef
        do 20 i = 1, nbdef
            call dcopy(neq, zr(idbase+(i-1)*neq), 1, zr(ltvec1), 1)
            call zerlag(neq, zi(iddeeq), vectr=zr(ltvec1))
            zr(ldres+iad+i-1) = ddot(neq,zr(ltvec1),1,zr(ltvec2),1)
 20     continue
!
 30 end do
!
! --- DESTRUCTION VECTEURS DE TRAVAIL
!
    call jedetr('&&'//pgc//'.BASEMO')
    call jedetr('&&'//pgc//'.VECT1')
    call jedetr('&&'//pgc//'.VECT2')
    call jedetr('&&'//pgc//'.VECT3')
!
    call jedema()
end subroutine
