subroutine te0283(option, nomte)
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
    implicit none
#include "jeveux.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/ntfcma.h"
#include "asterfort/ppgan2.h"
#include "asterfort/rcdiff.h"
#include "asterfort/rcfode.h"
#include "asterfort/rcvalb.h"
#include "asterfort/runge6.h"
#include "asterfort/uttgel.h"
!
    character(len=16) :: nomte, option
! ----------------------------------------------------------------------
!
!    - FONCTION REALISEE:  CALCUL DES VECTEURS RESIDUS
!                          OPTION : 'RESI_RIGI_MASS'
!                          ELEMENTS 3D ISO PARAMETRIQUES
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!
! THERMIQUE NON LINEAIRE
!
!
!
    character(len=2) :: typgeo
    integer :: icodre(1)
    real(kind=8) :: beta, lambda, theta, deltat, khi, tpg, tpgm
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), poids, hydrgm(27)
    real(kind=8) :: dtpgdx, dtpgdy, dtpgdz, rbid, chal(1), hydrgp(27)
    real(kind=8) :: tpsec, diff, err
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: jgano, nno, kp, npg1, i, itemps, ifon(3), l, ndim
    integer :: ihydr, ihydrp, itempr
    integer :: isechi, isechf, jgano2
    integer :: icomp, itempi, iveres, nnos
    integer :: npg2, ipoid2, ivf2, idfde2
! ----------------------------------------------------------------------
! PARAMETER ASSOCIE AU MATERIAU CODE
!
! --- INDMAT : INDICE SAUVEGARDE POUR LE MATERIAU
!
!CC      PARAMETER        ( INDMAT = 8 )
! ----------------------------------------------------------------------
!
! DEB ------------------------------------------------------------------
    call uttgel(nomte, typgeo)
    if ((lteatt('LUMPE','OUI')) .and. (typgeo.ne.'PY')) then
        call elrefe_info(fami='NOEU',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg2,jpoids=ipoid2,jvf=ivf2,jdfde=idfde2,jgano=jgano2)
    else
        call elrefe_info(fami='MASS',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg2,jpoids=ipoid2,jvf=ivf2,jdfde=idfde2,jgano=jgano2)
    endif
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PTEMPEI', 'L', itempi)
    call jevech('PCOMPOR', 'L', icomp)
    call jevech('PRESIDU', 'E', iveres)
!
    deltat = zr(itemps+1)
    theta = zr(itemps+2)
    khi = zr(itemps+3)
!
    if (zk16(icomp) (1:5) .eq. 'THER_') then
!
        call ntfcma(zk16(icomp), zi(imate), ifon)
!----
!   INITIALISATION THER_HYDR
!----
        if (zk16(icomp) (1:9) .eq. 'THER_HYDR') then
            call jevech('PHYDRPM', 'L', ihydr)
            call jevech('PHYDRPP', 'E', ihydrp)
            call jevech('PTEMPER', 'L', itempr)
            call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                        ' ', 'THER_HYDR', 0, ' ', [0.d0],&
                        1, 'CHALHYDR', chal, icodre, 1)
            do 150 kp = 1, npg2
                l = nno*(kp-1)
                hydrgm(kp)=0.d0
                do 160 i = 1, nno
                    hydrgm(kp)=hydrgm(kp)+zr(ihydr)*zr(ivf2+l+i-1)
160              continue
150          continue
!
        endif
!
        do 30 kp = 1, npg1
            l = (kp-1)*nno
            call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                        poids, dfdx, dfdy, dfdz)
            tpg = 0.d0
            dtpgdx = 0.d0
            dtpgdy = 0.d0
            dtpgdz = 0.d0
            do 10 i = 1, nno
                tpg = tpg + zr(itempi+i-1)*zr(ivf+l+i-1)
                dtpgdx = dtpgdx + zr(itempi+i-1)*dfdx(i)
                dtpgdy = dtpgdy + zr(itempi+i-1)*dfdy(i)
                dtpgdz = dtpgdz + zr(itempi+i-1)*dfdz(i)
10          continue
!
            call rcfode(ifon(2), tpg, lambda, rbid)
!
            do 20 i = 1, nno
                zr(iveres+i-1) = zr(iveres+i-1) + poids*theta*lambda* (dfdx(i)*dtpgdx+ dfdy(i)*dt&
                                 &pgdy+dfdz(i)*dtpgdz)
20          continue
30      continue
!
        do 60 kp = 1, npg2
            l = (kp-1)*nno
            call dfdm3d(nno, kp, ipoid2, idfde2, zr(igeom),&
                        poids, dfdx, dfdy, dfdz)
            tpg = 0.d0
            do 40 i = 1, nno
                tpg = tpg + zr(itempi+i-1)*zr(ivf2+l+i-1)
40          continue
! ---  RESOLUTION DE L EQUATION D HYDRATATION
!
            if (zk16(icomp) (1:9) .eq. 'THER_HYDR') then
                tpgm = 0.d0
                hydrgp(kp)=0.d0
                do 51 i = 1, nno
                    tpgm = tpgm + zr(itempr+i-1)*zr(ivf2+l+i-1)
51              continue
                call runge6(ifon(3), deltat, tpg, tpgm, hydrgm(kp),&
                            hydrgp(kp), err)
            endif
!
            call rcfode(ifon(1), tpg, beta, rbid)
            if (zk16(icomp) (1:9) .eq. 'THER_HYDR') then
! ---   THERMIQUE NON LINEAIRE AVEC HYDRATATION
                do 61 i = 1, nno
                    zr(iveres+i-1) = zr(iveres+i-1) + poids* ((beta- chal(1)*hydrgp(kp)) / deltat&
                                     &*khi*zr(ivf2+l+i-1))
61              continue
            else
! ---   THERMIQUE NON LINEAIRE SEULE
                do 50 i = 1, nno
                    zr(iveres+i-1) = zr(iveres+i-1) + poids*beta/ deltat*khi*zr(ivf2+l+i-1)
50              continue
            endif
60      continue
!
    else if (zk16(icomp) (1:5).eq.'SECH_') then
!
! --- SECHAGE
!
        if (zk16(icomp) (1:12) .eq. 'SECH_GRANGER' .or. zk16(icomp) (1: 10) .eq.&
            'SECH_NAPPE') then
            call jevech('PTMPCHI', 'L', isechi)
            call jevech('PTMPCHF', 'L', isechf)
        else
!          POUR LES AUTRES LOIS, PAS DE CHAMP DE TEMPERATURE
!          ISECHI ET ISECHF SONT FICTIFS
            isechi = itempi
            isechf = itempi
        endif
        do 70 kp = 1, npg1
            l = (kp-1)*nno
            call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                        poids, dfdx, dfdy, dfdz)
            tpg = 0.d0
            dtpgdx = 0.d0
            dtpgdy = 0.d0
            dtpgdz = 0.d0
            tpsec = 0.d0
            do 80 i = 1, nno
                tpg = tpg + zr(itempi+i-1)*zr(ivf+l+i-1)
                tpsec = tpsec + zr(isechf+i-1)*zr(ivf+l+i-1)
                dtpgdx = dtpgdx + zr(itempi+i-1)*dfdx(i)
                dtpgdy = dtpgdy + zr(itempi+i-1)*dfdy(i)
                dtpgdz = dtpgdz + zr(itempi+i-1)*dfdz(i)
80          continue
            call rcdiff(zi(imate), zk16(icomp), tpsec, tpg, diff)
!CDIR$ IVDEP
            do 90 i = 1, nno
                zr(iveres+i-1) = zr(iveres+i-1) + poids* ( theta*diff* (dfdx(i)*dtpgdx+dfdy(i) *d&
                                 &tpgdy+ dfdz(i)*dtpgdz))
90          continue
70      continue
        do 71 kp = 1, npg2
            l = (kp-1)*nno
            call dfdm3d(nno, kp, ipoid2, idfde2, zr(igeom),&
                        poids, dfdx, dfdy, dfdz)
            tpg = 0.d0
            do 81 i = 1, nno
                tpg = tpg + zr(itempi+i-1)*zr(ivf2+l+i-1)
81          continue
!CDIR$ IVDEP
            do 91 i = 1, nno
                zr(iveres+i-1) = zr(iveres+i-1) + poids* (1.d0/deltat* khi*zr(ivf2+l+i-1)*tpg)
91          continue
71      continue
    endif
    if (zk16(icomp) (1:9) .eq. 'THER_HYDR') call ppgan2(jgano2, 1, 1, hydrgp, zr(ihydrp))
! FIN ------------------------------------------------------------------
end subroutine
