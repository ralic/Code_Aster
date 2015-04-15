subroutine rcvala(jmat, nomat, phenom, nbpar, nompar,&
                  valpar, nbres, nomres, valres, icodre,&
                  iarret,nan)
    implicit none
#include "jeveux.h"
#include "asterfort/fointa.h"
#include "asterfort/rcvals.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/assert.h"
#include "asterc/r8nnem.h"
! ----------------------------------------------------------------------
! person_in_charge: jacques.pellet at edf.fr
    integer, intent(in) :: jmat, nbpar, nbres, iarret
    real(kind=8), intent(in) :: valpar(nbpar)
    real(kind=8), intent(out) :: valres(nbres)
    integer, intent(out) :: icodre(nbres)
    character(len=*), intent(in) :: nomat, phenom, nompar(nbpar), nomres(nbres)
    character(len=3), intent(in), optional :: nan
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
!
! But : Recuperation des valeurs d'une liste de coefficients d'une relation de
!       comportement pour un materiau donne.
!
!     arguments d'entree:
!        jmat      : adresse de la liste des materiaux codes
!        nomat     : nom du materiau dans le cas d'une liste de materiaux
!                    si = ' ', on exploite le premier de la liste
!        phenom    : nom du phenomene (mot cle facteur sans le "_FO")
!        nbpar     : nombre de parametres dans nompar(*) et valpar(*)
!        nompar(*) : noms des parametres(ex: 'TEMP', 'INST' )
!        valpar(*) : valeurs des parametres
!        nbres     : nombre de coefficients recherches
!                    (dimension des tableaux nomres(*), valres(*) et icodre(*)
!        nomres(*) : nom des resultats (ex: 'E','NU',... )
!                    tels qu'il figurent dans la commande DEFI_MATERIAU
!       iarret = 0 : on remplit icodre et on sort sans message.
!              = 1 : si un des parametres n'est pas trouve, on arrete
!                       en fatal en indiquant le nom de la maille.
!              = 2 : idem que 1 mais on n'indique pas la maille.
!       nan    = 'OUI' (defaut) : pour les parametres non trouves, on retourne valres = NaN
!              = 'NON' : pour les parametres non trouves, on ne modifie pas valres
!
!     arguments de sortie:
!        valres(*) : valeurs des resultats apres recuperation et interpolation
!        icodre(*) : pour chaque resultat, 0 si on a trouve, 1 sinon
!
! ----------------------------------------------------------------------
!   -- parameters associes au materiau code :
    integer :: lmat, lfct, lsup
    parameter  ( lmat = 9 , lfct = 10 , lsup = 2 )

    integer :: ires, icomp, ipi, iadzi, iazk24, nbobj, nbr, nbc, nbf, ivalk
    integer :: ivalr, ir, ipif, ik, nbmat, imat, kmat, inom
    character(len=8) :: nomail, nomi
    character(len=32) :: nomphe
    character(len=24) :: valk(2)
    real(kind=8) :: rundf
    aster_logical :: lnan
!  ---------------------------------------------------------------------

!   -- On est oblige de recopier phenom car il faut le tronquer
!      parfois a 10 avant de le comparer
    nomphe=phenom


!   -- initialisation de icodre(*) et valres(*) :
!   ---------------------------------------------
    rundf=r8nnem()
    lnan=.true.
    if (present(nan)) then
        ASSERT(nan.eq.'OUI' .or. nan.eq.'NON')
        if (nan.eq.'NON') lnan=.false.
    endif
    do ires = 1, nbres
        icodre(ires) = 1
        if (lnan) valres(ires) = rundf
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
        if (nbmat.gt.1) then
            call tecael(iadzi, iazk24)
            nomail = zk24(iazk24-1+3)(1:8)
            valk(1) = nomail
            valk(2) = nomres(1)
            call utmess('A', 'MODELISA9_3', nk=2, valk=valk)
        endif
        imat = jmat+zi(jmat+nbmat+1)
    endif
 9  continue


!   -- calcul de ipi (pour nomphe):
!   -------------------------------
    do icomp = 1, zi(imat+1)
        if (nomphe .eq. zk32(zi(imat)+icomp-1)) then
            ipi = zi(imat+2+icomp-1)
            goto 22
        endif
    enddo


!   -- selon la valeur de iarret on arrete ou non :
!   ----------------------------------------------
    if (iarret .ge. 1) then
        valk(1)=nomphe
        if (iarret .eq. 1) then
            call tecael(iadzi, iazk24)
            nomail = zk24(iazk24-1+3)(1:8)
            valk(2) = nomail
            call utmess('F', 'MODELISA9_75', nk=2, valk=valk)
        else
            call utmess('F', 'MODELISA9_74', sk=valk(1))
        endif
    endif
    goto 999


!   -- calcul de valres(*) :
!   -------------------------
22  continue
    nbobj = 0
    nbr = zi(ipi)
    nbc = zi(ipi+1)
    nbf = zi(ipi+2)
    ivalk = zi(ipi+3)
    ivalr = zi(ipi+4)
    do ires = 1, nbres
        do ir = 1, nbr
            if (nomres(ires) .eq. zk16(ivalk+ir-1)) then
                valres(ires) = zr(ivalr-1+ir)
                icodre(ires) = 0
                nbobj = nbobj + 1
                goto 32
            endif
        enddo
32      continue
    enddo

    if (nbobj .ne. nbres) then
        do ires = 1, nbres
            ipif = ipi+lmat-1
            do ik = 1, nbf
                if (nomres(ires) .eq. zk16(ivalk+nbr+nbc+ik-1)) then
                    ASSERT(zi(ipif+9).eq.1)
                    call fointa(ipif, nbpar, nompar, valpar, valres(ires))
                    icodre(ires) = 0
                endif
                ipif = ipif + lfct
                if (nomphe .eq. 'TRACTION') then
                    ipif = ipif + lsup
                else if (nomphe.eq. 'META_TRACT') then
                    ipif = ipif + lsup
                endif
            enddo
        enddo
    endif

999  continue

    call rcvals(iarret, icodre, nbres, nomres)


end subroutine
