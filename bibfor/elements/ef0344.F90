subroutine ef0344(nomte)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     CALCUL DE EFGE_ELNO
!     ------------------------------------------------------------------
    implicit none
#include "jeveux.h"
#include "asterfort/carapo.h"
#include "asterfort/jevech.h"
#include "asterfort/moytem.h"
#include "asterfort/pmavec.h"
#include "asterfort/porigi.h"
#include "asterfort/ptforp.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvgl.h"
#include "asterfort/vecma.h"
#include "asterfort/verift.h"
    character(len=16) :: nomte
!
!
    integer :: nbres
    parameter(nbres=2)
    integer :: lmater, jmat, nbmat, imat, icomp, nbpar, i, j, npg, nno, nc
    integer :: ncc, jeffo, iret, lsect, itype, lx
    integer :: lorien, jdepl, lforcr, lforcf
    real(kind=8) :: valres(nbres)
    integer :: codres(nbres)
    character(len=8) :: nompar
    character(len=16) :: messk(2), nomres(nbres)
    real(kind=8) :: valpar, zero, angs2, rad, e, g, a, rbid
    real(kind=8) :: xl, epsith
    real(kind=8) :: nu, fe(12), fi(12), flr(14), klv(105)
    real(kind=8) :: ulr(14), ugr(14), pgl(14, 14), klc(14, 14)
    real(kind=8) :: pgl1(3, 3), pgl2(3, 3)
!     ------------------------------------------------------------------
    data nomres/'E','NU'/
!     ------------------------------------------------------------------
!
!
! --- ------------------------------------------------------------------
! --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
    call jevech('PMATERC', 'L', lmater)
! --- ------------------------------------------------------------------
!     BLINDAGE : OPTION VALIDE AVEC UN SEUL PHENOMENE : ELAS
    jmat=zi(lmater)
    nbmat=zi(jmat)
!     UN SEUL MATERIAU
    if (nbmat .ne. 1) then
        messk(1)='EFGE_ELNO'
        call utmess('F', 'ELEMENTS4_59', sk=messk(1))
    endif
!     LE 1ER MATERIAU
    imat=jmat+zi(jmat+nbmat+1)
!     SEUL ELAS EST AUTORISE
    do icomp = 1, zi(imat+1)
        if (zk32(zi(imat)+icomp-1) .ne. 'ELAS') then
            messk(2)=zk32(zi(imat)+icomp-1)
            call utmess('F', 'ELEMENTS4_64', nk=2, valk=messk)
        endif
    end do
! --- ------------------------------------------------------------------
    nbpar=0
    nompar='  '
    valpar=0.d0
    zero=0.d0
    angs2=zero
    rad=zero
    do i = 1, nbres
        valres(i)=zero
    end do
!
    do i = 1, 3
        do j = 1, 3
            pgl1(i,j)=zero
            pgl2(i,j)=zero
        enddo
    end do
!
    npg=3
    call moytem('RIGI', npg, 1, '+', valpar,&
                iret)
!
    nbpar=1
    nompar='TEMP'
!
    call rcvalb('RIGI', 1, 1, '+', zi(lmater),&
                ' ', 'ELAS', nbpar, nompar, [valpar],&
                2, nomres, valres, codres, 1)
!
    e=valres(1)
    nu=valres(2)
    g=e/(2.d0*(1.d0+nu))
! --- ------------------------------------------------------------------
! --- RECUPERATION DES CARAC DES SECTIONS UTILES, LONGUEUR ET PGL
    nno = 2
    nc = 7
    ncc = 6
    call jevech('PCAGNPO', 'L', lsect)
    call jevech('PCAORIE', 'L', lorien)
    call jevech('PGEOMER', 'L', lx)
    call carapo(zr(lsect), zr(lx), zr(lorien), xl, pgl,&
                itype, a, rbid, rbid, rbid,&
                rbid, rbid, rbid, rbid, rbid,&
                rbid, rbid, rbid, rbid, rbid)
! --- ------------------------------------------------------------------
! --- CALCUL DE LA MATRICE DE RIGIDITE LOCALE
    call porigi(nomte, e, nu, xl, klv)
! --- ------------------------------------------------------------------
! --- MATRICE RIGIDITE LIGNE > MATRICE RIGIDITE CARRE
    call vecma(klv, 105, klc, 14)
!
    call jevech('PDEPLAR', 'L', jdepl)
    do i = 1, 14
        ugr(i)=zr(jdepl+i-1)
    end do
! --- VECTEUR DEPLACEMENT LOCAL  ULR = PGL * UGR
    call utpvgl(nno, nc, pgl, ugr, ulr)
! --- VECTEUR EFFORT       LOCAL  FLR = KLC * ULR
    call pmavec('ZERO', 14, klc, ulr, flr)
! --- ------------------------------------------------------------------
! --- TENIR COMPTE DES EFFORTS DUS A LA DILATATION
    call verift('RIGI', npg, 1, '+', zi(lmater),&
                epsth=epsith)
    do i = 1, 14
        ugr(i)=0.d0
    end do
    ugr(1)=-epsith*xl
    ugr(8)=-ugr(1)
! --- ------------------------------------------------------------------
! --- CALCUL DES FORCES INDUITES
    do i = 1, 7
        flr(i)=flr(i)-klc(i,1)*ugr(1)
        flr(i+7)=flr(i+7)-klc(i+7,1+7)*ugr(1+7)
    end do
! --- ------------------------------------------------------------------
! --- PRISE EN COMPTE DES EFFORTS REPARTIS
    call tecach('ONN', 'PFR1D1D', 'L', iret, iad=lforcr)
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
! --- ------------------------------------------------------------------
! --- PRISE EN COMPTE DES EFFORTS REPARTIS (SOUS FORME DE FONCTION)
    call tecach('ONN', 'PFF1D1D', 'L', iret, iad=lforcf)
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
! --- ------------------------------------------------------------------
! --- ARCHIVAGE
! ---    NOTER L INVERSION DU SIGNE DES EFFORTS SUR LE PREMIER NOEUD
!        (CONVENTION ADOPTEE/AL95-205)
    call jevech('PEFFORR', 'E', jeffo)
    do i = 1, 7
        zr(jeffo-1+i)=-flr(i)
        zr(jeffo-1+i+7)=flr(i+7)
    end do
!
!
end subroutine
