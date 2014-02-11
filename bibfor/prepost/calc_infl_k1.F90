subroutine calc_infl_k1(nomdb, sigmdb, tbscmb, prodef, londef, &
                        lrev, lmdb, matrev, matmdb, tempa,&
                        tempb, k1a, k1b, k1c)
    implicit none
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/wkvect.h"
#include "asterfort/coef_infl.h"
#include "asterfort/coef_poly.h"
#include "asterfort/utmess.h"
#include "asterfort/rcvale.h"
    integer :: nomdb
    real(kind=8) :: prodef, londef, lrev, lmdb
    real(kind=8) :: tempa, tempb
    real(kind=8) :: k1a, k1b, k1c
    real(kind=8) :: asxl
    character(len=8) :: matrev, matmdb
    character(len=19) :: sigmdb, tbscmb
! ======================================================================
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
!
!
! ======================================================================
!   POST_K_BETA
! ======================================================================
! --- BUT : CALCUL DES FACTEURS D'INTENSITE DE CONTRAINTES ELASTIQUES
!           AVEC LA METHODE DES COEFFICIENT D'INFLUENCE
! ======================================================================
! IN  : NOREV  : NOMBRE DE NOEUDS COTE REVETEMENT ----------------------
! --- : NOMDB  : NOMBRE DE NOEUDS COTE METAL DE BASE -------------------
! --- : SIGMRV : CONTRAINTES COTE REVETEMENT ---------------------------
! --- : SIGMDB : CONTRAINTES COTE METAL DE BASE ------------------------
! --- : TBSCRV : ABSCISSES CURVILIGNES COTE REVETEMENT -----------------
! --- : TBSCMB : ABSCISSES CURVILIGNES COTE METAL DE BASE --------------
! --- : PRODEF : PROFONDEUR DU DEFAUT ----------------------------------
! --- : LONDEF : LONGUEUR DU DEFAUT ------------------------------------
! --- : LREV   : EPAISSEUR DU REVETEMENT -------------------------------
! --- : LMDB   : EPAISSEUR DU METAL DE BASE ----------------------------
! --- : MATREV : NOM DU MATERIAU REVETEMENT ----------------------------
! --- : MATMDB : NOM DU MATERIAU METAL DE BASE -------------------------
! --- : TEMPA  : TEMPERATURE EN POINT A DE LA FISSURE ------------------
! --- : TEMPB  : TEMPERATURE EN POINTE B DE LA FISSURE -----------------
! OUT : K1A    : FACTEUR D'INTENSITE DE CONTRAINTES POINTE A -----------
! --- : K1B    : FACTEUR D'INTENSITE DE CONTRAINTES POINTE B -----------
! --- : K1C    : FACTEUR D'INTENSITE DE CONTRAINTES POINTE C -----------
! ======================================================================
! ======================================================================
    integer :: jsigmb, jabsmb, ific
    integer :: iexi, jcoein
    integer :: vali
    real(kind=8) :: zero
    real(kind=8) :: a, pi
    real(kind=8) :: ldefo, rtole
    real(kind=8) :: sigma(5)
    character(len=16) :: coeinf
! ======================================================================
! --- INITIALISATION DE PARAMETRES -------------------------------------
! ======================================================================
    parameter       ( zero   =  0.0d0 )
! ======================================================================
    call jemarq()
! ======================================================================
! --- DEFINITIONS ------------------------------------------------------
! ======================================================================
    coeinf = '&&OP0198.COEINF'
! ======================================================================
! --- INITIALISATIONS DES VECTEURS -------------------------------------
! ======================================================================
    call jeveuo(tbscmb, 'L', jabsmb)
    call jeveuo(sigmdb, 'L', jsigmb)
! ======================================================================
! --- INITIALISATIONS DES VARIABLES REPRESENTANT LES FACTEURS ----------
! --- D'INTENSITE ------------------------------------------------------
! ======================================================================
    k1a = zero
    k1b = zero
    k1c = zero
    rtole = 1.0d-10
! ======================================================================
! --- INITIALISATIONS DES VARIABLES NECESSAIRE AU CALCUL ---------------
! ======================================================================
    ldefo = zero
    a = prodef
    pi = r8pi()
! ======================================================================
! --- VERIFICATION DE LA COHERENCE DE LA PROFONDEUR DU DEFAUT ET -------
! --- DES ABSCISSES CURVILIGNES COTE METAL DE BASE ---------------------
! ======================================================================
!    ldefo = zr(jabsmb+nomdb-1)
!    if (abs(ldefo - prodef) .gt. rtole) then
!        call utmess('F', 'PREPOST_5')
!    endif
! ======================================================================
! --- CALCUL DES COEFFICIENTS D'INFLUENCE ------------------------------
! --- AUX POINTS A, B et C
! ======================================================================
!  15 : 3 points x 5 coefficients d'influence

    call jeexin(coeinf, iexi)
!
    if (iexi .gt. 0) then
       call jeveuo(coeinf, 'E', jcoein)
    else
       call wkvect(coeinf, 'V V R8', 15, jcoein)
    endif 
       call coef_infl(prodef, londef, lrev, matrev, matmdb, &
                      tempa, tempb, zr(jcoein))
! ======================================================================
! --- CALCUL DES COEFFICIENTS DU POLYNOME ------------------------------
! ======================================================================
! On verifie que l'on a bien 5 points

   if(nomdb.eq.5) then
      do ific = 1,5
         sigma(ific) = zr(jsigmb+ific-1)
      end do
   else
       vali=nomdb
       call utmess('F', 'PREPOST_6',si=vali)
!      ASSERT(.false.)
   endif
!
    call coef_poly(lrev,lmdb,zr(jabsmb),sigma)
!
    asxl=(prodef+lrev)/(lrev+lmdb)  

! ======================================================================
! --- CALCUL DU FACTEUR INTENSITE DE CONTRAINTE ------------------------
! --- AU POINT A -------------------------------------------------------
! ======================================================================

    k1a = sqrt(pi*a)*(sigma(1)*zr(jcoein)           + &
                      sigma(2)*zr(jcoein+1)*asxl    + &
                      sigma(3)*zr(jcoein+2)*asxl**2 + &
                      sigma(4)*zr(jcoein+3)*asxl**3 + &
                      sigma(5)*zr(jcoein+4)*asxl**4)

! ======================================================================
! --- CALCUL DU FACTEUR INTENSITE DE CONTRAINTE ------------------------
! --- AU POINT B -------------------------------------------------------
! ======================================================================

    k1b = sqrt(pi*a)*(sigma(1)*zr(jcoein+5)         + &
                      sigma(2)*zr(jcoein+6)*asxl    + &
                      sigma(3)*zr(jcoein+7)*asxl**2 + &
                      sigma(4)*zr(jcoein+8)*asxl**3 + &
                      sigma(5)*zr(jcoein+9)*asxl**4)

! ======================================================================
! --- CALCUL DU FACTEUR INTENSITE DE CONTRAINTE ------------------------
! --- AU POINT C -------------------------------------------------------
! ======================================================================
    k1c = sqrt(pi*a)*(sigma(1)*zr(jcoein+10)         + &
                      sigma(2)*zr(jcoein+11)*asxl    + &
                      sigma(3)*zr(jcoein+12)*asxl**2 + &
                      sigma(4)*zr(jcoein+13)*asxl**3 + &
                      sigma(5)*zr(jcoein+14)*asxl**4)
   call jedema()
! ======================================================================
end subroutine
