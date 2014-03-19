subroutine te0281(option, nomte)
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
! ----------------------------------------------------------------------
!
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'CHAR_THER_EVOLNI'
!                          ELEMENTS 3D ISOPARAMETRIQUES
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!
! THERMIQUE NON LINEAIRE LUMPE SANS HYDRATATION, NI SECHAGE
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! PARAMETRES D'APPEL
#include "jeveux.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/ntfcma.h"
#include "asterfort/rcdiff.h"
#include "asterfort/rcfode.h"
#include "asterfort/rcvalb.h"
#include "asterfort/uttgel.h"
!
    character(len=16) :: nomte, option
!
!
    integer :: icodre(1)
    character(len=2) :: typgeo
    real(kind=8) :: beta, dbeta, lambda, theta, deltat, tpg, dfdx(27)
    real(kind=8) :: dfdy(27), dfdz(27), poids, dtpgdx, dtpgdy, dtpgdz, dlambd
    real(kind=8) :: tpgbuf, tpsec, diff, chal(1), hydrpg(27)
    integer :: jgano, ipoids, ivf, idfde, igeom, imate, itemp, nno, kp, nnos
    integer :: npg, i, l, ifon(3), ndim, icomp, ivectt, ivecti
    integer :: itemps
    integer :: isechi, isechf, ihydr
    integer :: npg2, ipoid2, ivf2, idfde2
    logical :: lhyd
!
!====
! 1.1 PREALABLES: RECUPERATION ADRESSES FONCTIONS DE FORMES...
!====
    call uttgel(nomte, typgeo)
    if ((lteatt('LUMPE','OUI')) .and. (typgeo.ne.'PY')) then
        call elrefe_info(fami='NOEU',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg2,jpoids=ipoid2,jvf=ivf2,jdfde=idfde2,jgano=jgano)
    else
        call elrefe_info(fami='MASS',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg2,jpoids=ipoid2,jvf=ivf2,jdfde=idfde2,jgano=jgano)
    endif
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
!C====
! 1.2 PREALABLES LIES AUX RECHERCHES DE DONNEES GENERALES
!====
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PTEMPER', 'L', itemp)
    call jevech('PCOMPOR', 'L', icomp)
    call jevech('PVECTTR', 'E', ivectt)
    call jevech('PVECTTI', 'E', ivecti)
!
!====
! 1.3 PREALABLES LIES A L'HYDRATATION
!====
    deltat = zr(itemps+1)
    theta = zr(itemps+2)
!
    if (zk16(icomp)(1:5) .ne. 'SECH_') then
        call ntfcma(zk16(icomp), zi(imate), ifon)
    endif
!====
! 1.4 PREALABLES LIES A L'HYDRATATION
!====
    if (zk16(icomp) (1:9) .eq. 'THER_HYDR') then
        lhyd = .true.
        call jevech('PHYDRPM', 'L', ihydr)
        do 152 kp = 1, npg2
            l = nno*(kp-1)
            hydrpg(kp)=0.d0
            do 162 i = 1, nno
                hydrpg(kp)=hydrpg(kp)+zr(ihydr)*zr(ivf2+l+i-1)
162          continue
152      continue
!
        call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                    ' ', 'THER_HYDR', 0, ' ', [0.d0],&
                    1, 'CHALHYDR', chal, icodre, 1)
    else
        lhyd = .false.
    endif
    if (zk16(icomp)(1:5) .eq. 'THER_') then
!====
! 2. CALCULS DU TERME DE RIGIDITE DE L'OPTION
!====
!
        do 70 kp = 1, npg
            l = (kp-1)*nno
            call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                        poids, dfdx, dfdy, dfdz)
            tpg = 0.d0
            dtpgdx = 0.d0
            dtpgdy = 0.d0
            dtpgdz = 0.d0
            do 10 i = 1, nno
! CALCUL DE T- ET DE SON GRADIENT
                tpg = tpg + zr(itemp+i-1)*zr(ivf+l+i-1)
                dtpgdx = dtpgdx + zr(itemp+i-1)*dfdx(i)
                dtpgdy = dtpgdy + zr(itemp+i-1)*dfdy(i)
                dtpgdz = dtpgdz + zr(itemp+i-1)*dfdz(i)
10          continue
!
! CALCUL DES CARACTERISTIQUES MATERIAUX STD EN TRANSITOIRE UNIQUEMENT
! ON LES EVALUE AVEC TPG=T-
            tpgbuf = tpg
            call rcfode(ifon(2), tpgbuf, lambda, dlambd)
!
            do 40 i = 1, nno
                zr(ivectt+i-1) = zr(ivectt+i-1) - poids* (1.0d0-theta) *lambda* (dfdx(i)*dtpgdx+d&
                                 &fdy(i)*dtpgdy+ dfdz(i)* dtpgdz)
                zr(ivecti+i-1) = zr(ivecti+i-1) - poids* (1.0d0-theta) *lambda* (dfdx(i)*dtpgdx+d&
                                 &fdy(i)*dtpgdy+ dfdz(i)* dtpgdz)
40          continue
! FIN BOUCLE SUR LES PTS DE GAUSS
70      end do
!
!====
! 3. CALCULS DU TERME DE MASSE DE L'OPTION
!====
!
!
        do 140 kp = 1, npg2
            l = (kp-1)*nno
            call dfdm3d(nno, kp, ipoid2, idfde2, zr(igeom),&
                        poids, dfdx, dfdy, dfdz)
            tpg = 0.d0
            do 80 i = 1, nno
! CALCUL DE T- ET DE SON GRADIENT
                tpg = tpg + zr(itemp+i-1)*zr(ivf2+l+i-1)
80          continue
!
! CALCUL DES CARACTERISTIQUES MATERIAUX EN TRANSITOIRE UNIQUEMENT
! ON LES EVALUE AVEC TPG=T-
            tpgbuf = tpg
            call rcfode(ifon(1), tpgbuf, beta, dbeta)
            if (lhyd) then
! THER_HYDR
                do 81 i = 1, nno
                    zr(ivectt+i-1) = zr(ivectt+i-1) + poids* ((beta-  chal(1)*hydrpg(kp))* zr(ivf&
                                     &2+l+i-1)/deltat)
                    zr(ivecti+i-1) = zr(ivecti+i-1) + poids* ((dbeta*  tpg-chal(1)*hydrpg(kp))* z&
                                     &r(ivf2+l+i-1)/deltat)
81              continue
            else
! THER_NL
!
! CALCUL STD A 2 OUTPUTS (LE DEUXIEME NE SERT QUE POUR LA PREDICTION)
!
                do 110 i = 1, nno
                    zr(ivectt+i-1) = zr(ivectt+i-1) + poids*beta/ deltat*zr(ivf2+l+i-1)
                    zr(ivecti+i-1) = zr(ivecti+i-1) + poids*dbeta*tpg/ deltat*zr(ivf2+l+i-1)
110              continue
!
! ENDIF THER_HYDR
            endif
! FIN BOUCLE SUR LES PTS DE GAUSS
140      end do
!
! --- SECHAGE
!
    else if ((zk16(icomp) (1:5).eq.'SECH_')) then
        if (zk16(icomp) (1:12) .eq. 'SECH_GRANGER' .or. zk16(icomp) (1: 10) .eq.&
            'SECH_NAPPE') then
            call jevech('PTMPCHI', 'L', isechi)
            call jevech('PTMPCHF', 'L', isechf)
        else
!          POUR LES AUTRES LOIS, PAS DE CHAMP DE TEMPERATURE
!          ISECHI ET ISECHF SONT FICTIFS
            isechi = itemp
            isechf = itemp
        endif
        do 150 kp = 1, npg
            l = nno*(kp-1)
            call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                        poids, dfdx, dfdy, dfdz)
            tpg = 0.d0
            dtpgdx = 0.d0
            dtpgdy = 0.d0
            dtpgdz = 0.d0
            tpsec = 0.d0
            do 160 i = 1, nno
                tpg = tpg + zr( itemp+i-1)*zr(ivf+l+i-1)
                tpsec = tpsec + zr(isechi+i-1)*zr(ivf+l+i-1)
                dtpgdx = dtpgdx + zr(itemp+i-1)*dfdx(i)
                dtpgdy = dtpgdy + zr(itemp+i-1)*dfdy(i)
                dtpgdz = dtpgdz + zr(itemp+i-1)*dfdz(i)
160          continue
            call rcdiff(zi(imate), zk16(icomp), tpsec, tpg, diff)
!
            do 170 i = 1, nno
                zr(ivectt+i-1) = zr(ivectt+i-1) - poids* ( (1.0d0- theta)*diff* (dfdx(i)*dtpgdx+ &
                                 &dfdy(i)*dtpgdy+dfdz(i)* dtpgdz))
                zr(ivecti+i-1) = zr(ivectt+i-1)
170          continue
150      continue
        do 151 kp = 1, npg2
            l = nno*(kp-1)
            call dfdm3d(nno, kp, ipoid2, idfde2, zr(igeom),&
                        poids, dfdx, dfdy, dfdz)
            tpg = 0.d0
            dtpgdx = 0.d0
            dtpgdy = 0.d0
            dtpgdz = 0.d0
            tpsec = 0.d0
            do 161 i = 1, nno
                tpg = tpg + zr( itemp+i-1)*zr(ivf2+l+i-1)
                tpsec = tpsec + zr(isechi+i-1)*zr(ivf2+l+i-1)
                dtpgdx = dtpgdx + zr(itemp+i-1)*dfdx(i)
                dtpgdy = dtpgdy + zr(itemp+i-1)*dfdy(i)
                dtpgdz = dtpgdz + zr(itemp+i-1)*dfdz(i)
161          continue
            call rcdiff(zi(imate), zk16(icomp), tpsec, tpg, diff)
            do 171 i = 1, nno
                zr(ivectt+i-1) = zr(ivectt+i-1) + poids* (tpg/deltat* zr(ivf2+l+i-1))
                zr(ivecti+i-1) = zr(ivectt+i-1)
171          continue
151      continue
!
    endif
! FIN ------------------------------------------------------------------
end subroutine
