subroutine limend(nommaz, salt, nomres, forvie, limit)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/fointe.h"
#include "asterfort/fonbpa.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mess.h"
    logical :: limit
    character(len=*) :: nommaz, nomres, forvie
    real(kind=8) :: salt
! ----------------------------------------------------------------------
!     TEST PERMETTANT DE SAVOIR SI ON EST EN DESSOUS DE LA LIMITE
!     D'ENDURANCE POUR LA COURBE DE FATIGUE DEFINIE PAR LE
!     COMPORTEMENT FATIGUE ET LES MOTS CLES WOHLER OU MANSON_COFFIN
!
!     ARGUMENTS D'ENTREE:
!        NOMMAT : NOM UTILISATEUR DU MATERIAU
!        SALT   : VALEUR DE LA CONTRAINTE ALTERNEE A TESTER
!        NOMRES : NOM DU TYPE DE COURBE (WOHLER OU MANSON_COFFIN)
!     ARGUMENTS DE SORTIE:
!        LIMIT  : = .TRUE. SI SALT < LIMITE D'ENDURANCE =
!                   PREMIERE ABSCISSE DE LA COURBE DE WOHLER OU DE
!                   MANSON_COFFIN
!                 = .FALSE. SINON
!
!
!
!
    integer :: iret, ivalr, nbr, nbc, ivalk, nbk, nbf, ik, ivalf, iprol
    integer :: jprof, nbmx, np, ibid
    real(kind=8) :: vallim, nlimim
    character(len=10) :: nomphe
    character(len=8) :: nomfon, k8bid, nommat, nompf
    character(len=16) :: typfon
    character(len=24) :: chnom, cbid
!
    call jemarq()
!
    nommat = nommaz
    nomphe = 'FATIGUE   '
    limit = .false.
! NOMBRE DE PARAMETTRES MAX
    nbmx = 30
!
! LA DUREE DE VIE A 10^7
    nlimim = 1.d7
!
    if (nomres(1:6) .eq. 'WOHLER') then
!
!    DANS LE CAS OU LE MOT CLE EST WOHLER
!
        call jeexin(nommat//'.'//nomphe//'.VALR', iret)
        ASSERT(iret .ne. 0)
!
        call jeveuo(nommat//'.'//nomphe//'.VALR', 'L', ivalr)
        call jelira(nommat//'.'//nomphe//'.VALR', 'LONUTI', nbr, k8bid)
!
        call jelira(nommat//'.'//nomphe//'.VALC', 'LONUTI', nbc, k8bid)
        call jeexin(nommat//'.'//nomphe//'.VALK', iret)
        call jeveuo(nommat//'.'//nomphe//'.VALK', 'L', ivalk)
        call jelira(nommat//'.'//nomphe//'.VALK', 'LONUTI', nbk, k8bid)
!
!    NOMBRE DE FONCTIONS PRESENTES
!
        nbf = (nbk-nbr-nbc)/2
!
        do 10 ik = 1, nbf
            if (nomres .eq. zk8(ivalk-1+nbr+nbc+ik)) then
!         NOM DE LA FONCTION REPRESENTANT LA COURBE DE WOHLER
                nomfon = zk8(ivalk-1+nbr+nbc+nbf+ik)
!         VALEURS DE LA FONCTION REPRESENTANT LA COURBE DE WOHLER
                call jeveuo(nomfon//'           .VALE', 'L', ivalf)
!         PROLONGEMENT DE LA FONCTION REPRESENTANT LA COURBE DE WOHLER
                call jeveuo(nomfon//'           .PROL', 'L', iprol)
!         PROLONGEMENT A GAUCHE DE LA COURBE DE WOHLER EXCLU OU CONSTANT
                if ((zk24(iprol-1+5)(1:1).eq.'E') .or. (zk24(iprol-1+ 5)(1:1).eq.'C')) then
                    vallim=zr(ivalf)
                    if (salt .lt. vallim) then
                        limit=.true.
                    endif
                endif
                goto 20
            endif
10      continue
        call u2mess('F', 'MODELISA4_89')
20      continue
!
    else if (nomres(1:8) .eq. 'MANSON_C') then
!
!    DANS LE CAS OU LE MOT CLE EST MANSON_COFFIN
!
        call jeexin(nommat//'.'//nomphe//'.VALR', iret)
        ASSERT(iret .ne. 0)
!
        call jeveuo(nommat//'.'//nomphe//'.VALR', 'L', ivalr)
        call jelira(nommat//'.'//nomphe//'.VALR', 'LONUTI', nbr, k8bid)
!
        call jelira(nommat//'.'//nomphe//'.VALC', 'LONUTI', nbc, k8bid)
        call jeexin(nommat//'.'//nomphe//'.VALK', iret)
        call jeveuo(nommat//'.'//nomphe//'.VALK', 'L', ivalk)
        call jelira(nommat//'.'//nomphe//'.VALK', 'LONUTI', nbk, k8bid)
!
!    NOMBRE DE FONCTIONS PRESENTES
!
        nbf = (nbk-nbr-nbc)/2
!
        do 30 ik = 1, nbf
            if (nomres .eq. zk8(ivalk-1+nbr+nbc+ik)) then
!        NOM DE LA FONCTION REPRESENTANT LA COURBE DE MANSON_COFFIN
                nomfon = zk8(ivalk-1+nbr+nbc+nbf+ik)
!        VALEURS DE LA FONCTION REPRESENTANT LA COURBE DE MANSON_COFFIN
                call jeveuo(nomfon//'           .VALE', 'L', ivalf)
!        PROLONGEMENT DE LA FONCTION REPRESENTANT LA COURBE DE
!        MANSON_COFFIN
                call jeveuo(nomfon//'           .PROL', 'L', iprol)
!        PROLONGEMENT A GAUCHE DE LA COURBE DE MANSON_COFFIN EXCLU OU
!        CONSTANT
                if ((zk24(iprol-1+5)(1:1).eq.'E') .or. (zk24(iprol-1+ 5)(1:1).eq.'C')) then
                    vallim=zr(ivalf)
                    if (salt .lt. vallim) then
                        limit=.true.
                    endif
                endif
                goto 40
            endif
30      continue
        call u2mess('F', 'MODELISA4_91')
40      continue
!
    else if (nomres(1:8) .eq. 'FORM_VIE') then
!
!    DANS LE CAS OU LE MOT CLE N'EST PAS MANSON_COFFIN NI WOHLERS
        chnom(20:24) = '.PROL'
        chnom(1:19) = forvie
        call jeveuo(chnom, 'L', iprol)
        typfon = zk24(iprol-1+1)(1:8)
!
        call fonbpa(forvie, zk24(iprol), cbid, nbmx, np,&
                    nompf)
!
        if (typfon .eq. 'FONCTION') then
            chnom(20:24) = '.VALE'
            chnom(1:19) = forvie
!
            call jeveuo(chnom, 'L', jprof)
!
            call jelira(chnom, 'LONMAX', nbr, k8bid)
!
            nbf = nbr/2
!
            do 50 ik = 1, nbf
                chnom(20:24) = '.VALE'
                chnom(1:19) = forvie
                call jeveuo(chnom, 'L', ivalf)
!
                if ((zk24(iprol-1+5)(1:1).eq.'E') .or. (zk24(iprol-1+ 5)(1:1).eq.'C')) then
                    vallim=zr(ivalf)
                    if (salt .lt. vallim) then
                        limit=.true.
                    endif
                endif
                goto 60
50          continue
            call u2mess('F', 'MODELISA4_91')
60          continue
!
        else
! C'EST UNE FORMULE
!
! VERIFIER QUE LA FORMULE A LA VARIABLE NRUPT = N_F
            if ((nompf.ne. 'NBRUP') .or. (np .ne. 1)) then
                call u2mess('F', 'FATIGUE1_93')
            endif
!
            call fointe('F', forvie, np, nompf, nlimim,&
                        vallim, ibid)
!
            if (salt .lt. vallim) then
                limit=.true.
            endif
!
        endif
!
    endif
!
    call jedema()
end subroutine
