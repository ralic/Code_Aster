subroutine rcvalt(fami, kpg, ksp, poum, jmat, nomat, mfact,&
                  nbpar, nompar, valpar, &
                  nbres, valres, icodre, iarret)
use module_calcul, only : ca_jvcnom_, ca_nbcvrc_
implicit none
! person_in_charge: jacques.pellet at edf.fr
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/rcvarc.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/fointa.h"
#include "asterc/r8nnem.h"
! ------------------------------------------------------------------------
    character(len=*), intent(in) :: fami
    integer, intent(in)          :: kpg
    integer, intent(in)          :: ksp
    character(len=1), intent(in) :: poum
    integer, intent(in)          :: jmat
    character(len=*), intent(in) :: nomat, mfact
    integer, intent(in)          :: nbpar
    character(len=*), intent(in) :: nompar(nbpar)
    real(kind=8), intent(in)     :: valpar(nbpar)
    integer, intent(in)          :: nbres
    integer, intent(out)         :: icodre(*)
    real(kind=8), intent(out)    :: valres(*)
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
!  But : Recuperation de l'ENSEMBLE des parametres materiaux sous un mot cle facteur
!        La liste des parametres retournes (et la liste des code_retour) est
!        ordonnee selon l'ordre du catalogue (mot cle ORDRE_PARAM)
!        Les nbres premiers parametres de ORDRE_PARAM sont retournes.
!        Si un parametre n'est pas renseigne dans DEFI_MATERIAU, on retourne :
!          * icodre(i)=1
!          * valres(i)= NaN
!
!     arguments d'entree:
!       in   fami    : famille de point de gauss (rigi,mass,...)
!       in   kpg,ksp : numero du (sous)point de gauss
!       in   poum    : '+' /'-'
!       in   jmat    : adresse de la liste des materiaux codes (zi(imate))
!       in   nomat   : nom du materiau dans le cas d'une liste de materiaux
!                      si = ' ', on exploite le premier de la liste
!       in   mfact   : nom du mot cle facteur (ex 'VISCOCHAB')
!       in   nbpar   : nombre de parametres dans nompar et valpar
!       in   nompar  : noms des parametres(ex: 'TEMP' )
!       in   valpar  : valeurs des parametres
!       in   nbres   : dimension de valres et icodre
!       in   iarret  : comportement souhaite si mfact n'est pas renseigne :
!                = 0 : on remplit icodre(*)=1 et on sort sans message.
!                = 1 : on arrete <F> en indiquant le nom de la maille.
!                = 2 : idem que 1 mais on n'indique pas la maille.
!
!     arguments de sortie:
!       out  valres(*)  : valeur reelle du parametre (ou NaN)
!       out  icodre(*)  : 0 si on a trouve, 1 sinon
! ----------------------------------------------------------------------

    integer :: lmat, icomp, ipi, ipif, iadzi, iazk24, nbk, ivalk, nbr, nbc
    integer :: lfct, imat, nbmat, n1, k, posi, nmcs
    integer :: ier, jordr, jkord, ivalr, kr, kc, kf, ivalc
    integer :: nbpamx, nbpar2, nbpart, ipar, kmat, inom
    parameter (nbpamx=10)
    real(kind=8) ::  valvrc, rundf, valeur, valpa2(nbpamx)
    character(len=8) :: nompa2(nbpamx), novrc, nomi
    parameter  ( lmat = 9 , lfct = 10)
    character(len=32) :: valk
    character(len=8) :: nomail
    character(len=32) :: nomphe
!  ---------------------------------------------------------------------

    nomphe = mfact
    rundf=r8nnem()

!   -- on ne gere pas encore lsup
    ASSERT(nomphe.ne.'TRACTION')
    ASSERT(nomphe.ne.'META_TRAC')


!   -- initialisation de icodre(*) et valres(*) :
!   ---------------------------------------------
    do k=1,nbres
        icodre(k)=1
        valres(k)=rundf
    enddo


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


!   -- calcul de valres(*) :
!   -------------------------
11  continue
    nbr = zi(ipi)
    nbc = zi(ipi+1)
!   -- la routine n'a pas d'argument pour rendre des complexes :
    ASSERT(nbc.eq.0)
    nbk = zi(ipi+2)

    ivalk = zi(ipi+3)
    ivalr = zi(ipi+4)
    ivalc = zi(ipi+5)

!   -- la routine n'a de sens que pour un mot cle facteur ayant le mot cle ORDRE_PARAM :
    jordr=zi(ipi+6)
    jkord=zi(ipi+7)
    ASSERT(jordr.ne.1)
    ASSERT(jkord.ne.1)

    n1=zi(jkord-1+1)
    nmcs=zi(jkord-1+2)

!   -- si nbres est insuffisant :
    ASSERT(nbres.le.n1)


!   -- Recuperation des variables de commande :
!   -------------------------------------------
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

!   -- On ajoute les varc au debut de la liste des parametres
!      car fointa donne priorite aux derniers :
!   ----------------------------------------------------------
    nbpart=nbpar+nbpar2
    ASSERT(nbpart.le.nbpamx)
    do ipar=1,nbpar
        nompa2(nbpar2+ipar) = nompar(ipar)
        valpa2(nbpar2+ipar) = valpar(ipar)
    enddo


!   -- Boucle sur les mots cles simples et calcul de leurs valeurs :
!   ----------------------------------------------------------------
    do k=1,nmcs
!       -- posi : numero dans .ORDR :
        posi=zi(jkord-1+2+k)

        kr=zi(jkord-1+2+nmcs+k)
        kc=zi(jkord-1+2+2*nmcs+k)
        kf=zi(jkord-1+2+3*nmcs+k)

        if (kr.gt.0) then
            valeur = zr(ivalr-1+kr)
        elseif (kc.gt.0) then
            ASSERT(.false.)
        elseif (kf.gt.0) then
!           -- c'est un concept : une fonction ou une liste
            ipif = ipi+lmat+(kf-1)*lfct-1

!           -- cas d'une fonction :
            if (zi(ipif+9).eq.1) then
                call fointa(ipif, nbpart, nompa2, valpa2, valeur)

!           -- cas d'une table TRC :
            elseif (zi(ipif+9).eq.2) then
                ASSERT(.false.)

!           -- cas d'une liste de reels :
            elseif (zi(ipif+9).eq.3) then
                ASSERT(.false.)

!           -- cas d'une liste de fonctions :
            elseif (zi(ipif+9).eq.4) then
                ASSERT(.false.)
!               ipif2=zi(ipif)+3+lfct*(kv-1) ???
!               call fointa(ipif2, nbpart, nompa2, valpa2, valeur)
            else
                ASSERT(.false.)
            endif
        else
            ASSERT(.false.)
        endif
        valres(posi)=valeur
        icodre(posi)=0
    enddo

 999 continue
!
end subroutine
