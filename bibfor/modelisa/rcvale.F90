subroutine rcvale(nommaz, phenom, nbpar, nompar, valpar,&
                  nbres, nomres, valres, icodre, iarret)
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
#include "asterfort/assert.h"
#include "asterfort/fointe.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rcvals.h"
#include "asterfort/utmess.h"
    integer, intent(in) :: nbpar, nbres
    character(len=*), intent(in) :: phenom
    integer, intent(in) :: iarret
    character(len=*), intent(in) :: nommaz
    integer, intent(out) :: icodre(nbres)
    character(len=8), intent(in) :: nompar(nbpar), nomres(nbres)
    real(kind=8), intent(in) :: valpar(nbpar)
    real(kind=8), intent(out) :: valres(nbres)
! ----------------------------------------------------------------------
!     OBTENTION DE LA VALEUR VALRES D'UN "ELEMENT" D'UNE RELATION DE
!     COMPORTEMENT D'UN MATERIAU DONNE (NOUVELLE FORMULE RAPIDE)
!
!     ARGUMENTS D'ENTREE:
!        NOMMAT : NOM UTILISATEUR DU MATERIAU
!        PHENOM : NOM DU PHENOMENE (I.E. MOT CLE FACTEUR)
!        NBPAR  : NOMBRE DE PARAMETRES DANS NOMPAR ET VALPAR
!        NOMPAR : NOMS DES PARAMETRES(EX: 'TEMP' )
!        VALPAR : VALEURS DES PARAMETRES
!        NBRES  : NOMBRE DE RESULTATS
!        NOMRES : NOM DES RESULTATS (EX: E,NU,... )
!                 TELS QU'IL FIGURENT DANS LA COMMANDE DEFI_MATERIAU
!        IARRET : 0/1/2  COMPORTEMENT VOULU EN CAS DE PROBLEME
!           /0 : PAS DE MESSAGE D'ERREUR
!           /1 : ERREUR FATALE AVEC IDENTIFICATION DE LA MAILLE
!           /2 : ERREUR FATALE SANS IDENTIFICATION DE LA MAILLE
!           PLUS PRECISEMENT :
!           * SI LE PARAMETRE (REEL, COMPLEXE OU FONCTION)
!             N'A PAS ETE FOURNI PAR L'UTILISATEUR :
!               SI IARRET > 0  => ERREUR FATALE
!               SI IARRET = 0  => CODE RETOUR > 0
!           * SI LE PARAMETRE EST UNE FONCTION FOURNIE PAR
!             L'UTILISATEUR MAIS QUE SON EVALUATION ECHOUE :
!             => ERREUR FATALE DANS TOUS LES CAS
!
!     ARGUMENTS DE SORTIE:
!       VALRES : VALEURS DES RESULTATS APRES RECUPERATION
!                OU EVALUATION DE LA FONCTION
!       ICODRE : POUR CHAQUE RESULTAT, 0 SI ON A TROUVE, 1 SINON
!
!
!
!
    integer :: nbmx, nbresp, ires, ier, nbr, nbc, nbk, iret
    integer ::   nbobj, nbf, ir, ik
    parameter        ( nbmx=30 )
    integer :: nbfp
    real(kind=8) :: valrep(nbmx)
    logical :: change
    integer :: icodr2(nbmx)
    character(len=2) :: kstop
    character(len=10) :: phen, phepre
    character(len=8) :: matpre, nomrep(nbmx), nomfop(nbmx)
    character(len=10) :: nomphe
    character(len=8) :: nommat
    real(kind=8), pointer :: valr(:) => null()
    character(len=8), pointer :: valk(:) => null()
    save  matpre,phepre,nbfp,nbresp,nomrep,valrep,icodr2,nomfop
!
    call jemarq()
    nommat = nommaz
    phen = phenom
    kstop='F '
!
    ASSERT(iarret.ge.0 .and. iarret.le.2)
!
!
! --- TESTS: CELA A-T-IL CHANGE ?
    change = .false.
    if (nbres .gt. nbmx) then
        call utmess('F', 'MODELISA6_94', sk=nommat)
    endif
    if (nommat .ne. matpre) change = .true.
    if (phen .ne. phepre) change = .true.
    if (nbres .ne. nbresp) change = .true.
    do 100 ires = 1, nbres
        if (nomres(ires) .ne. nomrep(ires)) change = .true.
100  end do
!
!
    if (.not.change) then
        do 110 ires = 1, nbres
            valres(ires) = valrep(ires)
            icodre(ires) = icodr2(ires)
110      continue
        if (nbfp .eq. 0) goto 9999
!
        do 120 ires = 1, nbres
            if (nomfop(ires) .ne. ' ') then
                call fointe(kstop, nomfop(ires), nbpar, nompar, valpar,&
                            valres(ires), ier)
                ASSERT(ier.eq.0)
                icodre(ires) = 0
            else
                icodre(ires) = 1
            endif
120      continue
!
!
    else
        nomphe = phen
        call jeexin(nommat//'.'//nomphe//'.VALR', iret)
        if (iret .eq. 0) then
            do 113 ires = 1, nbres
                icodre(ires) = 1
113          continue
            goto 999
        endif
!
        call jeveuo(nommat//'.'//nomphe//'.VALR', 'L', vr=valr)
        call jelira(nommat//'.'//nomphe//'.VALR', 'LONUTI', nbr)
        call jelira(nommat//'.'//nomphe//'.VALC', 'LONUTI', nbc)
        call jeveuo(nommat//'.'//nomphe//'.VALK', 'L', vk8=valk)
        call jelira(nommat//'.'//nomphe//'.VALK', 'LONUTI', nbk)
        do 130 ires = 1, nbres
            icodre(ires) = 1
            nomfop(ires) = ' '
130      continue
        nbobj = 0
        do 150 ir = 1, nbr
            do 140 ires = 1, nbres
                if (nomres(ires) .eq. valk(ir)) then
                    valres(ires) = valr(ir)
                    icodre(ires) = 0
                    nbobj = nbobj + 1
                endif
140          continue
150      continue
        if (nbobj .ne. nbres) then
            nbf = (nbk-nbr-nbc)/2
            do 170 ires = 1, nbres
                do 160 ik = 1, nbf
                    if (nomres(ires) .eq. valk(nbr+nbc+ik)) then
                        nomfop(ires) = valk(nbr+nbc+nbf+ik)
                        call fointe(kstop, nomfop(ires), nbpar, nompar, valpar,&
                                    valres(ires), ier)
                        ASSERT(ier.eq.0)
                        icodre(ires) = 0
                    endif
160              continue
170          continue
        else
            nbf = 0
        endif
!
!       -- SAUVEGARDE DES VALEURS POUR LE PROCHAIN APPEL :
        matpre = nommat
        phepre = phen
        nbfp = nbf
        nbresp = nbres
        do 180 ires = 1, nbresp
            nomrep(ires) = nomres(ires)
            valrep(ires) = valres(ires)
            icodr2(ires) = icodre(ires)
180      continue
!
    endif
999  continue
9999  continue
!
    call rcvals(iarret, icodre, nbres, nomres)
!
    call jedema()
end subroutine
