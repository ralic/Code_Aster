subroutine rcadlv(fami, kpg, ksp, poum, jmat, nomat, mfact, msimp, &
                  nbpar, nompar, valpar, jadr, nbres, icodre, iarret)
use module_calcul, only : ca_jvcnom_, ca_nbcvrc_
implicit none
! person_in_charge: jacques.pellet at edf.fr
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/rcvals.h"
#include "asterfort/rcvarc.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/fointa.h"
! -----------------------------------------------------------------
    character(len=*), intent(in) :: fami
    integer, intent(in)          :: kpg
    integer, intent(in)          :: ksp
    character(len=1), intent(in) :: poum
    integer, intent(in)          :: jmat
    character(len=*), intent(in) :: nomat,mfact,msimp
    integer, intent(in)          :: nbpar
    character(len=*), intent(in) :: nompar(nbpar)
    real(kind=8), intent(in)     :: valpar(nbpar)
    integer, intent(out)         :: icodre
    integer, intent(out)         :: jadr
    integer, intent(out)         :: nbres
    integer, intent(in)          :: iarret

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
!  Recuperation de l'adresse jeveux (dans zr) des coefficients materiau
!  correspondant a DEFI_MATERIAU/MFACT/MSIMP
!  Cette routine doit etre utilisee quand le mot cle MFACT/MSIMP
!  correspond a une liste de reels (UMAT/LISTE_COEF par exemple)
!
!     arguments d'entree:
!       in   fami    : famille de point de gauss ('RIGI','MASS',...)
!       in   kpg,ksp : numero du (sous)point de gauss
!       in   poum    : '+' /'-'
!       in   jmat    : adresse de la liste des materiaux codes (zi(imate))
!       in   nomat   : nom du materiau dans le cas d'une liste de materiaux
!                      si = ' ', on exploite le premier de la liste
!       in   mfact   : nom du mot cle facteur (ex 'UMAT')
!       in   msimp   : nom du mot cle simple (ex 'LISTE_COEF')
!                      tels qu'il figurent dans la commande DEFI_MATERIAU
!       in   nbpar   : nombre de parametres dans nompar et valpar
!       in   nompar  : noms des parametres(ex: 'TEMP' )
!       in   valpar  : valeurs des parametres
!
!     arguments de sortie:
!       out  jadr    : adresse dans zr de la liste de reels
!       out  nbres   : nombre de valeurs dans zr(jadr)
!       out  icodre  : 0 si on a trouve, 1 sinon
! ----------------------------------------------------------------------
!
    integer :: lmat, icomp, ipi, ipif, iadzi, iazk24, nbk, ivalk, ik, nbr, nbc
    integer :: lfct, imat, nbmat, code, kv, nbv, ipif2, kmat, inom
    real(kind=8) :: valres
    integer :: nbpamx, nbpar2, nbpart, ipar, ier
    parameter (nbpamx=10)
    real(kind=8) :: valpa2(nbpamx), valvrc
    character(len=8) :: nompa2(nbpamx), novrc
    parameter  ( lmat = 9 , lfct = 10)
    character(len=32) :: valk
    character(len=8) :: nomail, nomi
    character(len=32) :: nomphe
!   --------------------------------------------------------------------------

    icodre = 1
    nomphe = mfact


!   -- Calcul de imat
!      Si nomat est fourni , on explore l'entete de la sd mater_code pour
!      trouver le "bon" materiau de la la liste
!   ----------------------------------------------------------------------
    nbmat=zi(jmat)
    if (nomat(1:1) .ne. ' ') then
        do kmat = 1, nbmat
            inom=zi(jmat+kmat)
            nomi=zk8(inom)
            if (nomi .eq. nomat) then
                imat = jmat+zi(jmat+nbmat+kmat)
                goto 9
            endif
        enddo
        call utmess('F', 'CALCUL_45', sk=nomat)
    else
        ASSERT(nbmat.eq.1)
        imat = jmat+zi(jmat+nbmat+1)
    endif
 9  continue


!   -- calcul de ipi (pour nomphe):
!   -------------------------------
    do icomp = 1, zi(imat+1)
        if (nomphe .eq. zk32(zi(imat)+icomp-1)) then
            ipi = zi(imat+2+icomp-1)
            goto 11
        endif
    enddo

!   -- selon la valeur de iarret on arrete ou non :
!   -----------------------------------------------
    if (iarret .ge. 1) then
        valk = nomphe
        call utmess('F+', 'CALCUL_46', sk=valk)
        if (iarret .eq. 1) then
            call tecael(iadzi, iazk24)
            nomail = zk24(iazk24-1+3)(1:8)
            valk = nomail
            call utmess('F+', 'CALCUL_47', sk=valk)
        endif
        call utmess('F', 'VIDE_1')
    endif
    goto 999


!   -- calcul de jadr et de zr(jadr:)
!   -----------------------------------------------
11  continue
    nbr = zi(ipi)
    nbc = zi(ipi+1)
    nbk = zi(ipi+2)
    ivalk = zi(ipi+3)
    do ik = 1, nbk
        if (msimp .eq. zk16(ivalk+nbr+nbc+ik-1)) then
            icodre = 0
            ipif = ipi + lmat + (ik-1)*lfct -1
            ASSERT(zi(ipif+9).eq.3 .or. zi(ipif+9).eq.4)
            code = zi(zi(ipif))
            ASSERT ((code.eq.-1).or.(code.eq.-2))


!           -- 1. Cas d'une liste de reels  :
!           ----------------------------------
            if (code.eq.-1) then
                jadr=zi(zi(ipif)+1)


!           -- 2. Cas d'une liste de fonctions
!           ----------------------------------
            else
                jadr=zi(zi(ipif)+1)
                nbv=zi(zi(ipif)+2)

!               -- 2.1 Recuperation des variables de commande :
!               -----------------------------------------------
                nbpar2 = 0
                do ipar=1,ca_nbcvrc_
                    novrc=zk8(ca_jvcnom_-1+ipar)
                    call rcvarc(' ', novrc, poum, fami, kpg,&
                                ksp, valvrc, ier)
                    if (ier .eq. 0) then
                        nbpar2=nbpar2+1
                        nompa2(nbpar2)=novrc
                        valpa2(nbpar2)=valvrc
                    endif
                enddo

!               -- 2.2 On ajoute les varc au debut de la liste des parametres
!                  car fointa donne priorite aux derniers :
!               --------------------------------------------------------------
                nbpart=nbpar+nbpar2
                ASSERT(nbpart.le.nbpamx)
                do ipar=1,nbpar
                    nompa2(nbpar2+ipar) = nompar(ipar)
                    valpa2(nbpar2+ipar) = valpar(ipar)
                enddo

!               -- 2.3 On evalue les fonctions de la liste :
!               ----------------------------------------------------------
                do kv=1,nbv
                    ipif2=zi(ipif)+3+lfct*(kv-1)
                    call fointa(ipif2, nbpart, nompa2, valpa2, valres)
                    zr(jadr-1+1+kv)=valres
                enddo
            endif
            goto 999
        endif
    enddo

    call rcvals(iarret, [icodre], 1, msimp)

999 continue
    nbres=nint(zr(jadr))
    jadr=jadr+1
end subroutine
