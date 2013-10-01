subroutine calir5(noma, lisrel, nono2, nuno2, jcoor,&
                  idecal, jconb, jcocf, jconu)
    implicit none
#include "jeveux.h"
#include "asterfort/afrela.h"
#include "asterfort/assert.h"
#include "asterfort/imprel.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
!
    character(len=19) :: lisrel
    character(len=8) :: nono2, noma
    integer :: nuno2, jconb, jcocf, jconu, jcoor
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
! person_in_charge: jacques.pellet at edf.fr
! BUT : ECRIRE LES RELATIONS LINEAIRES LIANT LES TRANSLATIONS D'UN NOEUD
!       "MASSIF" AVEC LES TRANSLATIONS ET ROTATIONS D'1 NOEUD "COQUE"
!       (AFFE_CHAR_MECA/LIAISON_MAIL + TYPE_RACCORD='MASSIF_COQUE'
! ======================================================================
!
    real(kind=8) :: beta
    character(len=2) :: typlag
    character(len=4) :: fonree
    character(len=4) :: typcoe
!
    real(kind=8) :: coefr(29), direct(3*29), coef1
    real(kind=8) :: a(3), n2(3), an2(3)
    complex(kind=8) :: cbid, betac
    character(len=8) :: kbeta, noeud(28), ddl(28), nono1, cmp
    integer :: dimens(28), nbterm, ndim
    integer :: n1, ino1, nuno1, k, idec, idecal
    cbid = dcmplx(0.d0, 0.d0)
! ----------------------------------------------------------------------
!
    beta=0.0d0
    betac=(0.0d0,0.0d0)
    kbeta=' '
    typcoe='REEL'
    fonree='REEL'
    typlag='12'
    ndim=3
!
!     N1 : NOMBRE DE NOEUDS DE LA MAILLE "COQUE" EN FACE DE N2
    n1=zi(jconb-1+nuno2)
    ASSERT(n1.ge.3 .and. n1.le.9)
!
    nbterm=1+n1*ndim
    ASSERT(nbterm.le.28)
!
!
!     CALCUL DES COORDONNEES DU POINT A (COQUE) EN FACE DE N2 (MASSIF) :
!     ET DU VECTEUR AN2 :
!     ------------------------------------------------------------------
    do k = 1, ndim
        a(k)=0.d0
        do ino1 = 1, n1
            nuno1=zi(jconu+idecal-1+ino1)
            coef1=zr(jcocf+idecal-1+ino1)
            a(k)=a(k)+coef1*zr(jcoor-1+3*(nuno1-1)+k)
        end do
        n2(k)=zr(jcoor-1+3*(nuno2-1)+k)
        an2(k)=n2(k)-a(k)
    end do
!
    do k = 1, nbterm
        dimens(k)=0
    end do
!
!
    do k = 1, ndim
        if (k .eq. 1) cmp='DX'
        if (k .eq. 2) cmp='DY'
        if (k .eq. 3) cmp='DZ'
!
        noeud(1)=nono2
        ddl(1)=cmp
        coefr(1)=-1.d0
!
        idec=2
        do ino1 = 1, n1
            nuno1=zi(jconu+idecal-1+ino1)
            coef1=zr(jcocf+idecal-1+ino1)
            call jenuno(jexnum(noma//'.NOMNOE', nuno1), nono1)
            noeud(idec)=nono1
            ddl(idec)=cmp
            coefr(idec)=coef1
            idec=idec+1
        end do
!
        do ino1 = 1, n1
            nuno1=zi(jconu+idecal-1+ino1)
            coef1=zr(jcocf+idecal-1+ino1)
            call jenuno(jexnum(noma//'.NOMNOE', nuno1), nono1)
            noeud(idec-1+1)=nono1
            noeud(idec-1+2)=nono1
            if (k .eq. 1) then
                ddl(idec-1+1)='DRY'
                ddl(idec-1+2)='DRZ'
                coefr(idec-1+1)=+coef1*an2(3)
                coefr(idec-1+2)=-coef1*an2(2)
            else if (k.eq.2) then
                ddl(idec-1+1)='DRZ'
                ddl(idec-1+2)='DRX'
                coefr(idec-1+1)=+coef1*an2(1)
                coefr(idec-1+2)=-coef1*an2(3)
            else if (k.eq.3) then
                ddl(idec-1+1)='DRX'
                ddl(idec-1+2)='DRY'
                coefr(idec-1+1)=+coef1*an2(2)
                coefr(idec-1+2)=-coef1*an2(1)
            endif
            idec=idec+2
        end do
        ASSERT(idec.eq.nbterm+1)
!
        call afrela(coefr, [cbid], ddl, noeud, dimens,&
                    direct, nbterm, beta, betac, kbeta,&
                    typcoe, fonree, typlag, 1.d-6, lisrel)
        call imprel('LIAISON_MAIL-MASSIF_COQUE', nbterm, coefr, ddl, noeud,&
                    beta)
!
    end do
!
!
!
end subroutine
