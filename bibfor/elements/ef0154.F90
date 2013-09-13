subroutine ef0154(nomte)
! ======================================================================
    implicit none
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/lonele.h"
#include "asterfort/matrot.h"
#include "asterfort/pmavec.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvgl.h"
#include "asterfort/verift.h"
!
    character(len=16) :: nomte
! ----------------------------------------------------------------------
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
!     CALCUL  EFGE_ELNO
!
!
    real(kind=8) :: pgl(3, 3), klc(6, 6)
    real(kind=8) :: ugr(6), ulr(6), flr(6)
    integer :: codres
    character(len=4) :: fami
    character(len=8) :: nomail, materi
    character(len=16) :: ch16
    logical :: lteimp
    real(kind=8) :: a, epsth, e, r8bid, xfl1, xfl4, xl, xrig
    integer :: i, j, jdepl, jeffo
    integer :: lmater, lorien, lsect, iret, lx, nc, nno, iadzi, iazk24
!     ------------------------------------------------------------------
!
    lteimp=.false.
    materi = ' '
    nno=2
    nc=3
    fami='RIGI'
!
    if ((nomte.ne.'MECA_BARRE') .and. (nomte.ne.'MECA_2D_BARRE')) then
        ch16=nomte
        call utmess('F', 'ELEMENTS2_42', sk=ch16)
    endif
!
!     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
    call jevech('PMATERC', 'L', lmater)
!
    call verift(fami, 1, 1, '+', zi(lmater),&
                materi, 'ELAS', 1, epsth, iret)
!
    call rcvalb(fami, 1, 1, '+', zi(lmater),&
                ' ', 'ELAS', 0, ' ', r8bid,&
                1, 'E', e, codres, 1)
    if (epsth .ne. 0.d0) lteimp=.true.
!
!     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
    call jevech('PGEOMER', 'L', lx)
    lx=lx-1
!
    if (nomte .eq. 'MECA_BARRE') then
        call lonele(zr(lx), 3, xl)
    else if (nomte.eq.'MECA_2D_BARRE') then
        call lonele(zr(lx), 2, xl)
    endif
!
    if (xl .eq. 0.d0) then
        call tecael(iadzi, iazk24)
        nomail=zk24(iazk24-1+3)(1:8)
        call utmess('F', 'ELEMENTS2_43', sk=nomail)
    endif
!
!     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
    call jevech('PCAGNBA', 'L', lsect)
    a=zr(lsect)
!
!     --- RECUPERATION DES ORIENTATIONS ALPHA,BETA,GAMMA ---
    call jevech('PCAORIE', 'L', lorien)
!     --- MATRICE DE ROTATION PGL
    call matrot(zr(lorien), pgl)
!
!     --- RECUPERATION DES DEPLACEMENTS OU DES VITESSES ----
    do 10 i = 1, 6
        ugr(i)=0.d0
10  end do
!
!
! ON RECUPERE DES DEPLACEMENTS
!
    call jevech('PDEPLAR', 'L', jdepl)
    if (nomte .eq. 'MECA_BARRE') then
        do 20 i = 1, 6
            ugr(i)=zr(jdepl+i-1)
20      continue
    else if (nomte.eq.'MECA_2D_BARRE') then
        ugr(1)=zr(jdepl+1-1)
        ugr(2)=zr(jdepl+2-1)
        ugr(4)=zr(jdepl+3-1)
        ugr(5)=zr(jdepl+4-1)
    endif
!
!
!     --- VECTEUR DANS REPERE LOCAL  ULR = PGL * UGR
!
    call utpvgl(nno, nc, pgl, ugr, ulr)
!
!     --- RIGIDITE ELEMENTAIRE ---
    do 40 i = 1, 6
        do 30 j = 1, 6
            klc(i,j)=0.d0
30      continue
40  end do
!
    xrig=e*a/xl
    klc(1,1)=xrig
    klc(1,4)=-xrig
    klc(4,1)=-xrig
    klc(4,4)=xrig
!
!
!        --- VECTEUR EFFORT LOCAL  FLR = KLC * ULR
    call pmavec('ZERO', 6, klc, ulr, flr)
!
!        --- TENIR COMPTE DES EFFORTS DUS A LA DILATATION ---
    if (lteimp) then
!              --- CALCUL DES FORCES INDUITES ---
        xfl1=-epsth*e*a
        xfl4=-xfl1
        flr(1)=flr(1)-xfl1
        flr(4)=flr(4)-xfl4
    endif
!
    call jevech('PEFFORR', 'E', jeffo)
    zr(jeffo)=-flr(1)
    zr(jeffo+1)=flr(4)
!
end subroutine
