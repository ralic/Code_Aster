subroutine utno3d(ifm, niv, nsomm, ifa, tymvol,&
                  igeom, xn, yn, zn, jac,&
                  idfdx, idfdy, hf, poids3, npgf,&
                  noe)
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: olivier.boiteau at edf.fr
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  UTILITAIRE DE CALCUL DE LA NORMALE A UNE
!                          FACE EN SES NOEUDS. POUR AERER TE0003
! REMARQUE : IL Y A DOUBLE EMPLOI AVEC UTNORM ET CALNOR
!            ET CERTAINEMENT AVEC D'AUTRES PROGRAMMES
!
! IN IFM/NIV  : PARAMETRES D'IMPRESSION
! IN NSOMM    : NOMBRE DE SOMMETS DE LA FACE
! IN IFA      : NUMERO DE FACE
! IN TYMVOL   : TYPE DE MAILLE VOLUMIQUE
! IN IGEOM    : ADRESSE JEVEUX DE LA GEOMETRIE
! IN IDFDX/DY : ADRESSES JEVEUX DES DERIVEES DES FFORMES DE LA FACE IFA
! IN POIDS3   : POIDS DE NEWTON-COTES DE LA FACE.
! IN NPGF     : NBRE POINTS GAUSS DE LA FACE (=NSOMM EN NEWTON-COTES)
! IN NOE      : TABLEAU NUMEROS NOEUDS FACE ET PAR TYPE D'ELEMENT 3D
! OUT XN/YN/ZN/JAC : COMPOSANTES DE LA NORMALE ET JACOBIEN (* POIDS3)
! OUT HF      : SURFACE DE LA FACE
!   -------------------------------------------------------------------
!     SUBROUTINES APPELLEES:
!       CALCUL HF: UTHK.
!       ENVIMA:R8MIEM.
!
!     FONCTIONS INTRINSEQUES:
!       SQRT.
!   -------------------------------------------------------------------
!     ASTER INFORMATIONS:
!       20/09/01 (OB): CREATION POUR SIMPLIFIER TE0003.F.
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterc/r8miem.h"
#include "asterfort/assert.h"
#include "asterfort/uthk.h"
    integer :: ifm, niv, nsomm, ifa, tymvol, igeom, idfdx, idfdy, npgf
    integer :: noe(9, 6, 4)
    real(kind=8) :: jac(9), xn(9), yn(9), zn(9), hf, poids3(9)
!
!
! DECLARATION VARIABLES LOCALES
    integer :: in, jn, iino, jjno, i, j, ipg, kdec, idec, jdec, ixk, iyk
    real(kind=8) :: sx(9, 9), sy(9, 9), sz(9, 9), aux, ovfl
    character(len=16) :: nomteb
!
! INIT.
    ovfl = r8miem()
    nomteb = ' '
!
!  CALCUL DES PRODUITS VECTORIELS OMI VECTORIEL OMJ
    do in = 1, nsomm
        iino = noe(in,ifa,tymvol)
        i = igeom + 3*(iino-1)
        do jn = 1, nsomm
            jjno = noe(jn,ifa,tymvol)
            j = igeom + 3*(jjno-1)
            sx(in,jn) = zr(i+1) * zr(j+2) - zr(i+2) * zr(j+1)
            sy(in,jn) = zr(i+2) * zr(j) - zr(i) * zr(j+2)
            sz(in,jn) = zr(i) * zr(j+1) - zr(i+1) * zr(j)
        end do
    end do
!
! CALCUL DES NORMALES AUX SOMMETS IPG DE LA FACE IFA ET DE SON
! DIAMETRE
!
    hf = 0.d0
    do ipg = 1, npgf
        kdec = 2*(ipg-1)*nsomm
        ixk = idfdx+kdec
        iyk = idfdy+kdec
        xn(ipg) = 0.0d0
        yn(ipg) = 0.0d0
        zn(ipg) = 0.0d0
        do i = 1, nsomm
            idec = 2*(i-1)
            do j = 1, nsomm
                jdec = 2*(j-1)
                xn(ipg)=xn(ipg)+zr(ixk+idec)*zr(iyk+jdec)*sx(i,j)
                yn(ipg)=yn(ipg)+zr(ixk+idec)*zr(iyk+jdec)*sy(i,j)
                zn(ipg)=zn(ipg)+zr(ixk+idec)*zr(iyk+jdec)*sz(i,j)
            end do
        end do
!   JACOBIEN
        aux = sqrt(xn(ipg)*xn(ipg)+yn(ipg)*yn(ipg)+ zn(ipg)*zn(ipg))
        jac(ipg) = aux*poids3(ipg)
!
! NORMALISATION A L'UNITE DES COMPOSANTES DE LA NORMALE
        ASSERT(abs(aux).gt.ovfl)
        aux = -1.d0/aux
        xn(ipg) = xn(ipg)*aux
        yn(ipg) = yn(ipg)*aux
        zn(ipg) = zn(ipg)*aux
    end do
!
! DIAMETRE DE LA FACE
    if (niv .eq. 2) write(ifm,*)
    call uthk(nomteb, zr(igeom), hf, 0, niv,&
              noe=noe, nsomm=nsomm, tymvol=tymvol, ifa=ifa)
!
! AFFICHAGES
    if (niv .eq. 2) then
        write(ifm,*)'NUMERO DE FACE ',ifa
        write(ifm,*)'NOMBRE DE SOMMETS/PGAUSS ',nsomm,npgf
        write(ifm,*)'CONNECTIQUE ',(noe(i,ifa,tymvol),i=1,nsomm)
        write(ifm,*)'XN  ',(xn(i),i=1,npgf)
        write(ifm,*)'YN  ',(yn(i),i=1,npgf)
        write(ifm,*)'ZN  ',(zn(i),i=1,npgf)
        write(ifm,*)'JAC ',(jac(i),i=1,npgf)
    endif
end subroutine
