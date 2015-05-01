subroutine poutre_modloc(modloc, lnomv, nbnomv, lvaleur, valeur,&
                         arret, retour)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! person_in_charge: jean-luc.flejou at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jevech.h"
#include "asterfort/tecach.h"
!
    character(len=*), intent(in) :: modloc
    integer, intent(in) :: nbnomv
    character(len=*), intent(in) :: lnomv(nbnomv)
    real(kind=8), intent(out),optional :: lvaleur(nbnomv)
    real(kind=8), intent(out),optional :: valeur
    character(len=*), intent(in),optional :: arret
    integer, intent(out),optional :: retour
!
! --------------------------------------------------------------------------------------------------
!
!              UTILITAIRE POUR LES CARACTERISTIQUES DE POUTRE
!
! --------------------------------------------------------------------------------------------------
!  IN
!       modloc  : mode local de la poutre : CAGNPO , CAGNP1 , CAGNP2, CAGEPO , CAGEP1
!       lnomv   : liste des noms des caractéristiques que l'on souhaite récupérer
!                   ils doivent être dans l'ordre des data
!       nbnomv  : nombre de caractéristiques a récupérer
!       arret   : code pour tecach : 'NNN','ONN', ...
!  OUT
!       lvaleur : liste des valeurs des caractéristiques
!       valeur  : valeur de la caractéristique (nbnomv=1)
!       retour  : retour du tecach
!
! --------------------------------------------------------------------------------------------------
!
!
    integer :: dimdata
    parameter  (dimdata=25)
    real(kind=8) ::     r8data(dimdata)
    character(len=8) :: k8data(dimdata)
!
    integer :: isect, ii, jj, jj0, iret
    integer :: nbvaleur, nbdata
!
! --------------------------------------------------------------------------------------------------
!
    ASSERT( UN_PARMI2(lvaleur,valeur) )
    nbvaleur = nbnomv
    if ( present(valeur)  ) then
        ASSERT( nbnomv.eq.1 )
    endif
    ASSERT( nbvaleur .le. dimdata )
!
! --------------------------------------------------------------------------------------------------
    if ( present(arret) ) then
        ASSERT( present(retour) )
        if      (modloc(1:5) .eq. 'CAGNP') then
            call tecach(arret, 'PCAGNPO', 'L', iret, iad=isect)
        else if (modloc(1:5) .eq. 'CAGEP') then
            call tecach(arret, 'PCAGEPO', 'L', iret, iad=isect)
        else
            ASSERT( .false. )
        endif
        retour = iret
        r8data(:) = 0.0d0
        if ( retour .ne. 0 ) goto 999
    else
        if      (modloc(1:5) .eq. 'CAGNP') then
            call jevech('PCAGNPO', 'L', isect)
        else if (modloc(1:5) .eq. 'CAGEP') then
            call jevech('PCAGEPO', 'L', isect)
        else
            ASSERT( .false. )
        endif
    endif
    isect = isect - 1
!
    nbdata = 0
    if      (modloc .eq. 'CAGNPO') then
!       copie conforme des modes locaux : POU_D_TGM   POU_D_EM   POU_D_TG   POU_D_T  POU_C_T 
!                                         et POU_D_T_GD
        nbdata = 25
        k8data(1:nbdata) = (/   'A1      ', 'IY1     ', 'IZ1     ', 'AY1     ', 'AZ1     ', &
                                'EY1     ', 'EZ1     ', 'JX1     ', 'RY1     ', 'RZ1     ', &
                                'RT1     ', 'JG1     ', 'A2      ', 'IY2     ', 'IZ2     ', &
                                'AY2     ', 'AZ2     ', 'EY2     ', 'EZ2     ', 'JX2     ', &
                                'RY2     ', 'RZ2     ', 'RT2     ', 'JG2     ', 'TVAR    ' /)
    else if (modloc .eq. 'CAGNP1') then
!       copie conforme des modes locaux : POU_D_TGM   POU_D_TG
        nbdata = 11
        k8data(1:nbdata) = (/   'A1      ', 'IY1     ', 'IZ1     ', 'AY1     ', 'AZ1     ', &
                                'EY1     ', 'EZ1     ', 'JX1     ', 'JG1     ', 'IYR21   ', &
                                'IZR21   '/)
    else if (modloc .eq. 'CAGNP2') then
!       copie conforme des modes locaux des poutres : POU_D_TGM
        nbdata = 25
        k8data(1:nbdata) = (/   'A1      ', 'IY1     ', 'IZ1     ', 'AY1     ', 'AZ1     ', &
                                'EY1     ', 'EZ1     ', 'JX1     ', 'RY1     ', 'RZ1     ', &
                                'RT1     ', 'JG1     ', 'IYR21   ', 'IZR21   ', 'IY2     ', &
                                'IZ2     ', 'AY2     ', 'AZ2     ', 'EY2     ', 'EZ2     ', &
                                'JX2     ', 'RY2     ', 'RZ2     ', 'RT2     ', 'TVAR    ' /)
    else if (modloc .eq. 'CAGEPO') then
!       copie conforme des modes locaux : POU_D_E  POU_C_T  POU_D_TG  POU_D_T
        nbdata = 13
        k8data(1:nbdata) = (/   'HY1     ', 'HZ1     ', 'EPY1    ', 'EPZ1    ', 'HY2     ', &
                                'HZ2     ', 'EPY2    ', 'EPZ2    ', 'R1      ', 'EP1     ', &
                                'R2      ', 'EP2     ', 'TSEC    ' /)
    else if (modloc .eq. 'CAGEP1') then
!       copie conforme des modes locaux : POU_D_E  POU_C_T  POU_D_TG  POU_D_T  POU_D_EM  POU_D_TGM
!                                         et TUYAUX
        nbdata = 2
        k8data(1:nbdata) = (/   'R1      ', 'EP1     ' /)
    else
        write(*,*) 'CAG[N|E]P[0-2] '//modloc
        ASSERT(.false.)
    endif
!
!   Recherche des caractéristiques
    jj0 = 1
    cii: do ii = 1, nbvaleur
        do jj = jj0, nbdata
            if (lnomv(ii) .eq. k8data(jj)) then
                r8data(ii) = zr(isect+jj)
                jj0 = jj + 1
                cycle cii
            endif
        enddo
        write(*,*) 'CAGNPO '//modloc//' <'//lnomv(ii)//'>'
        ASSERT( .false. )
    enddo cii
!
! --------------------------------------------------------------------------------------------------
!   traitements particuliers : passage du centre de gravite au centre de torsion
    if (modloc(1:5) .eq. 'CAGNP') then
        do ii = 1, nbvaleur
            if      (lnomv(ii).eq. 'EY1') then
                r8data(ii) = -r8data(ii)
            else if (lnomv(ii).eq. 'EZ1') then
                r8data(ii) = -r8data(ii)
            else if (lnomv(ii).eq. 'EY2') then
                r8data(ii) = -r8data(ii)
            else if (lnomv(ii).eq. 'EZ2') then
                r8data(ii) = -r8data(ii)
            endif
        enddo
    endif
!
999 continue
!
! --------------------------------------------------------------------------------------------------
!   debug
!     do ii = 1, nbnomv
!         write(*,90) lnomv(ii), r8data(ii)
!     enddo
! 90  format(a,' : ',e18.10)
!
    if ( present(lvaleur) ) then
        do ii = 1, nbvaleur
            lvaleur(ii) = r8data(ii)
        enddo
    else
        valeur = r8data(1)
    endif
!
end subroutine
