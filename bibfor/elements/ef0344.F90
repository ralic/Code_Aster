subroutine ef0344(nomte)
!
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
!
! --------------------------------------------------------------------------------------------------
!
!     CALCUL DE EFGE_ELNO
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/lonele.h"
#include "asterfort/matrot.h"
#include "asterfort/moytem.h"
#include "asterfort/pmavec.h"
#include "asterfort/porigi.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/ptforp.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvgl.h"
#include "asterfort/vecma.h"
#include "asterfort/verift.h"
!
    character(len=16) :: nomte
!
! --------------------------------------------------------------------------------------------------
!
    integer :: lmater, jmat, nbmat, imat, icomp, nbpar, i, npg, nno, nc
    integer :: ncc, jeffo, iret, itype
    integer :: lorien, jdepl, lforcr, lforcf
    character(len=8) :: nompar
    character(len=32) :: messk(2)
    real(kind=8) :: valpar, angs2, rad, e, g, a
    real(kind=8) :: xl, epsith
    real(kind=8) :: nu, fe(12), fi(12), flr(14), klv(105)
    real(kind=8) :: ulr(14), ugr(14), pgl(14, 14), klc(14, 14)
    real(kind=8) :: pgl1(3, 3), pgl2(3, 3)
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nbres=2
    real(kind=8) :: valres(nbres)
    integer :: codres(nbres)
    character(len=16) :: nomres(nbres)
    data nomres/'E','NU'/
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nb_cara = 2
    real(kind=8) :: vale_cara(nb_cara)
    character(len=8) :: noms_cara(nb_cara)
    data noms_cara /'A1','TVAR'/
!
! --------------------------------------------------------------------------------------------------
!
!   recuperation des caracteristiques materiaux
    call jevech('PMATERC', 'L', lmater)
!
!   option valide avec un seul phenomene : elas
    jmat=zi(lmater)
    nbmat=zi(jmat)
!   un seul materiau
    if (nbmat .ne. 1) then
        messk(1)='EFGE_ELNO'
        call utmess('F', 'ELEMENTS4_59', sk=messk(1))
    endif
!   le 1er materiau
    imat=jmat+zi(jmat+nbmat+1)
!   seul elas est autorise
    do icomp = 1, zi(imat+1)
        if (zk32(zi(imat)+icomp-1) .ne. 'ELAS') then
            messk(2)=zk32(zi(imat)+icomp-1)
            call utmess('F', 'ELEMENTS4_64', nk=2, valk=messk)
        endif
    enddo
!
    nbpar=0
    nompar='  '
    valpar=0.d0
    angs2=0.0d0
    rad=0.0d0
    valres(:)=0.0d0
!
    pgl1(:,:)=0.0d0
    pgl2(:,:)=0.0d0
!
    npg=3
    call moytem('RIGI', npg, 1, '+', valpar, iret)
    nbpar=1
    nompar='TEMP'
!
    call rcvalb('RIGI', 1, 1, '+', zi(lmater), ' ', 'ELAS', nbpar, nompar, [valpar],&
                2, nomres, valres, codres, 1)
!
    e=valres(1)
    nu=valres(2)
    g=e/(2.d0*(1.d0+nu))
!
!   recuperation des carac des sections utiles, longueur et pgl
    nno = 2
    nc = 7
    ncc = 6
    call jevech('PCAORIE', 'L', lorien)
    call matrot(zr(lorien), pgl)
    xl = lonele()
    call poutre_modloc('CAGNPO', noms_cara, nb_cara, lvaleur=vale_cara)
    a = vale_cara(1)
    itype = nint(vale_cara(2))
!
!   calcul de la matrice de rigidite locale
    call porigi(nomte, e, nu, xl, klv)
!   matrice rigidite ligne > matrice rigidite carre
    call vecma(klv, 105, klc, 14)
!
    call jevech('PDEPLAR', 'L', jdepl)
    do i = 1, 14
        ugr(i)=zr(jdepl+i-1)
    enddo
!   vecteur deplacement local  ULR = PGL * UGR
    call utpvgl(nno, nc, pgl, ugr, ulr)
!   vecteur effort local  FLR = KLC * ULR
    call pmavec('ZERO', 14, klc, ulr, flr)
!
!   tenir compte des efforts dus a la dilatation
    call verift('RIGI', npg, 1, '+', zi(lmater), epsth_=epsith)
    ugr(:)=0.d0
    ugr(1)=-epsith*xl
    ugr(8)=-ugr(1)
!   calcul des forces induites
    do i = 1, 7
        flr(i)=flr(i)-klc(i,1)*ugr(1)
        flr(i+7)=flr(i+7)-klc(i+7,1+7)*ugr(1+7)
    enddo
!   prise en compte des efforts repartis
    call tecach('ONO', 'PFR1D1D', 'L', iret, iad=lforcr)
    if (lforcr .ne. 0) then
        call ptforp(itype, 'CHAR_MECA_FR1D1D', nomte, a, a,&
                    xl, rad, angs2, 1, nno,&
                    ncc, pgl, pgl1, pgl2, fe,&
                    fi)
        do i = 1, 6
            flr(i)=flr(i)-fe(i)
            flr(i+7)=flr(i+7)-fe(i+6)
        enddo
    endif
!   prise en compte des efforts repartis (sous forme de fonction)
    call tecach('ONO', 'PFF1D1D', 'L', iret, iad=lforcf)
    if (lforcf .ne. 0) then
        call ptforp(itype, 'CHAR_MECA_FF1D1D', nomte, a, a,&
                    xl, rad, angs2, 1, nno,&
                    ncc, pgl, pgl1, pgl2, fe,&
                    fi)
        do i = 1, 6
            flr(i)=flr(i)-fe(i)
            flr(i+7)=flr(i+7)-fe(i+6)
        enddo
    endif
!
!   archivage. Inversion du signe des efforts sur le premier noeud
    call jevech('PEFFORR', 'E', jeffo)
    do i = 1, 7
        zr(jeffo-1+i)=-flr(i)
        zr(jeffo-1+i+7)=flr(i+7)
    enddo
!
end subroutine
