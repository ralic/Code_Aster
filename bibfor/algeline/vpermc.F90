subroutine vpermc(lmasse, lraide, nbprop, vecp, fr,&
                  am, excl, omecor, ernorm)
    implicit none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/mcmult.h'
    include 'asterfort/u2mesr.h'
    include 'asterfort/wkvect.h'
    integer :: lmasse, lraide, nbprop, excl(*)
    complex(kind=8) :: vecp(*)
    real(kind=8) :: fr(*), am(*), omecor, ernorm(*)
!     ------------------------------------------------------------------
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
! ======================================================================
!     CALCUL DE LA NORME D'ERREUR MODALE
!  ( IE NORME D'ERREUR SUR LES VALEURS ET VECTEURS PROPRES COMPLEXES.)
!     ------------------------------------------------------------------
!     PROBLEME QUADRATIQUE:
!
!                   !! LRAIDE * VECP  - VALP * LMASSE * VECP !!
!       ERNORM   =     -------------------------------------
!                           !! LRAIDE * VECP !!
!     ------------------------------------------------------------------
!     REFERENCE:
!     ------------------------------------------------------------------
! IN  LMASSE : IS : DESCRIPTEUR MATRICE DE "MASSE"
! IN  LRAIDE : IS : DESCRIPTEUR MATRICE DE "RAIDEUR"
! IN  NBPROP : IS : NOMBRE DE VALEURS ET DE VECTEURS PROPRES
! IN  VECP   : C16 : TABLEAU DES VECTEURS PROPRES
! IN  VALP   : R8 : TABLEAU DES VALEURS PROPRES
! IN  EXCL   : IS : TABLEAU DES NON-EXCLUS
! IN  FCORIG : R8 : FREQUENCE MODE DE CORPS RIGIDE
! OUT ERNORM : R8 : TABLEAU DES NORMES D'ERREUR
!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    real(kind=8) :: xseuil
    real(kind=8) :: valr
    real(kind=8) :: ami, fri
    complex(kind=8) :: freq2, anorm1, anorm2
    integer :: iaux1, iaux2, iaux4, i, j, ivec, neq
!
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    xseuil = omecor
    neq = zi(lmasse+2)
!
    call wkvect('&&VPERMC.TAMP.PROV_1', 'V V C', neq, iaux1)
    call wkvect('&&VPERMC.TAMP.PROV_2', 'V V C', neq, iaux2)
!      CALL WKVECT('&&VPERMC.TAMPON.PROV_3' ,'V V C',NEQ,IAUX3)
    call wkvect('&&VPERMC.TYPEDDL      ', 'V V I', neq, iaux4)
!
    do 1 i = 1, nbprop
!
        ivec=(i-1)*neq+1
        do 10 j = 0, neq-1
            vecp(ivec+j) = vecp(ivec+j) * excl(j+1)
10      continue
!
        ami = am(i)
        fri = fr(i)
        if (abs(ami) .eq. 1.d0) then
            ernorm(i)= 1.d+70
            valr = 1.0d70
            call u2mesr('A', 'ALGELINE4_74', 1, valr)
        else
            freq2=dcmplx(fri,ami*fri*2.d0)
            call mcmult('ZERO', lraide, vecp(ivec), zc(iaux1), 1,&
                        .false.)
            call mcmult('ZERO', lmasse, vecp(ivec), zc(iaux2), 1,&
                        .false.)
!
            do 2 j = 0, neq-1
                zc(iaux2+j)=zc(iaux1+j)-freq2*zc(iaux2+j)
 2          continue
!
!           --- ON PREND LA NORME EUCLIDIENNE ---
            anorm1 = dcmplx(0.d0,0.d0)
            anorm2 = dcmplx(0.d0,0.d0)
            do 3 j = 0, neq-1
                anorm1 = anorm1 + (dconjg(zc(iaux1+j))*zc(iaux1+j))* excl(j+1)
                anorm2 = anorm2 + (dconjg(zc(iaux2+j))*zc(iaux2+j))* excl(j+1)
 3          continue
            if (abs(freq2) .gt. xseuil) then
                if (anorm1 .ne. dcmplx(0.d0,0.d0)) then
                    ernorm(i)= sqrt( abs(anorm2 / anorm1) )
                else
                    ernorm(i)= 1.d+70
                endif
            else
                ernorm(i) = abs(freq2) * sqrt( abs(anorm2) )
            endif
!
        endif
 1  end do
!
    call jedetr('&&VPERMC.TAMP.PROV_1')
    call jedetr('&&VPERMC.TAMP.PROV_2')
!      CALL JEDETR('&&VPERMC.TAMPON.PROV_3' )
    call jedetr('&&VPERMC.TYPEDDL      ')
!
    call jedema()
end subroutine
