subroutine lrcnme(chanom, nochmd, nomamd, nomaas, nomgd,&
                  typent, nbcmpv, ncmpva, ncmpvm, iinst,&
                  numpt, numord, inst, crit, prec,&
                  nrofic, codret)
!_____________________________________________________________________
!
! person_in_charge: nicolas.sellenet at edf.fr
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
!     LECTURE D'UN CHAMP AUX NOEUDS - FORMAT MED
!     -    -       -         -               --
!-----------------------------------------------------------------------
!     ENTREES:
!        CHANOM : NOM ASTER DU CHAMP A LIRE
!        NOCHMD : NOM MED DU CHAMP DANS LE FICHIER
!        NOMAMD : NOM MED DU MAILLAGE LIE AU CHAMP A LIRE
!                  SI ' ' : ON SUPPOSE QUE C'EST LE PREMIER MAILLAGE
!                           DU FICHIER
!        TYPENT : TYPE D'ENTITE DU CHAMP
!                (MED_NOEUD=3,MED_MAILLE=0,MED_NOEUD_MAILLE=4)
!        NOMAAS : NOM ASTER DU MAILLAGE
!        NOMGD  : NOM DE LA GRANDEUR ASSOCIEE AU CHAMP
!        NBCMPV : NOMBRE DE COMPOSANTES VOULUES
!                 SI NUL, ON LIT LES COMPOSANTES A NOM IDENTIQUE
!        NCMPVA : LISTE DES COMPOSANTES VOULUES POUR ASTER
!        NCMPVM : LISTE DES COMPOSANTES VOULUES DANS MED
!        IINST  : 1 SI LA DEMANDE EST FAITE SUR UN INSTANT, 0 SINON
!        NUMPT  : NUMERO DE PAS DE TEMPS EVENTUEL
!        NUMORD : NUMERO D'ORDRE EVENTUEL DU CHAMP
!        INST   : INSTANT EVENTUEL
!        CRIT   : CRITERE SUR LA RECHERCHE DU BON INSTANT
!        PREC   : PRECISION SUR LA RECHERCHE DU BON INSTANT
!        NROFIC : NUMERO NROFIC LOGIQUE DU FICHIER MED
!     SORTIES:
!        CODRET : CODE DE RETOUR (0 : PAS DE PB, NON NUL SI PB)
!_____________________________________________________________________
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "jeveux.h"
#include "asterfort/as_mfdfdi.h"
#include "asterfort/as_mfdnfc.h"
#include "asterfort/as_mfdnfd.h"
#include "asterfort/as_mficlo.h"
#include "asterfort/as_mfiope.h"
#include "asterfort/cnscno.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/lrcame.h"
#include "asterfort/ulisog.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=*) :: chanom
    character(len=*) :: ncmpva, ncmpvm
    character(len=8) :: nomaas, nomgd
    character(len=8) :: crit
    character(len=*) :: nochmd, nomamd
!
    integer :: nrofic, typent
    integer :: nbcmpv
    integer :: iinst, numpt, numord
    integer :: codret
!
    real(kind=8) :: inst
    real(kind=8) :: prec
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'LRCNME' )
!
    integer :: iaux, iret, jcmpva, nbcmpa, nbcham, i, nbcmp, jcmp, junit
    integer :: ibid, nseqca, tycha, idfimd, j
    integer :: jnocmp, ncmprf, ubid
    parameter(ubid=1)
    integer :: unbid(ubid)
    integer :: edlect
    parameter (edlect=0)
!
    character(len=1) :: saux01
    character(len=8) :: saux08, parbid
    character(len=19) :: chamn
    character(len=19) :: chamns, ligbid
    character(len=24) :: optbid
    character(len=64) :: nomcha
    character(len=200) :: nofimd
    character(len=255) :: kfic
    logical :: ttt
!
!====
! 1. ALLOCATION D'UN CHAM_NO_S  (CHAMNS)
!====
!
    call jemarq()
! 1.1. ==> REPERAGE DES CARACTERISTIQUES DE CETTE GRANDEUR
!
    call jenonu(jexnom ( '&CATA.GD.NOMGD', nomgd ), iaux)
    if (iaux .eq. 0) then
        call utmess('F', 'MED_65')
    endif
    call jeveuo(jexnom ( '&CATA.GD.NOMCMP', nomgd ), 'L', jnocmp)
    call jelira(jexnom ( '&CATA.GD.NOMCMP', nomgd ), 'LONMAX', ncmprf)
!
! 1.2. ==> ALLOCATION DU CHAM_NO_S
!
!               1234567890123456789
    chamns = '&&      .CNS.MED   '
    chamns(3:8) = nompro
!
    call jeexin(ncmpva, iret)
    if (iret .gt. 0) then
        call jeveuo(ncmpva, 'L', jcmpva)
        call jelira(ncmpva, 'LONMAX', nbcmpa)
        if (nbcmpa .le. ncmprf) then
            do 20 i = 1, nbcmpa
                ttt=.false.
                do 30 j = 1, ncmprf
                    if (zk8(jcmpva+i-1) .eq. zk8(jnocmp+j-1)) ttt= .true.
30              continue
                if (.not.ttt) then
                    call utmess('F', 'MED_66')
                endif
20          continue
        else
            call utmess('F', 'MED_70')
        endif
!
    else
        call ulisog(nrofic, kfic, saux01)
        if (kfic(1:1) .eq. ' ') then
            call codent(nrofic, 'G', saux08)
            nofimd = 'fort.'//saux08
        else
            nofimd = kfic(1:200)
        endif
        call as_mfiope(idfimd, nofimd, edlect, iret)
        call as_mfdnfd(idfimd, nbcham, iret)
        do 780 i = 1, nbcham
            call as_mfdnfc(idfimd, i, nbcmp, iret)
            call wkvect('&&LRCNME.NOMCMP_K16', 'V V K16', nbcmp, jcmp)
            call wkvect('&&LRCNME.UNITCMP', 'V V K16', nbcmp, junit)
            call as_mfdfdi(idfimd, i, nomcha, tycha, zk16(jcmp),&
                           zk16(junit), nseqca, iret)
            if (nomcha .eq. nochmd) then
                ncmprf=nbcmp
                call wkvect('&&LRCNME.NOMCMP_K8', 'V V K8', nbcmp, jnocmp)
                do 778 j = 1, nbcmp
                    zk8(jnocmp+j-1)=zk16(jcmp+j-1)(1:8)
778              continue
                call jedetr('&&LRCNME.NOMCMP_K16')
                call jedetr('&&LRCNME.UNITCMP')
                goto 780
            endif
            call jedetr('&&LRCNME.NOMCMP_K16')
            call jedetr('&&LRCNME.UNITCMP')
780      continue
        call as_mficlo(idfimd, iret)
!
    endif
!
!====
! 2. LECTURE
!====
!
    ligbid=' '
    optbid=' '
    parbid=' '
    call lrcame(nrofic, nochmd, nomamd, nomaas, ligbid,&
                optbid, parbid, 'NOEU', typent, unbid,&
                unbid, nbcmpv, ncmpva, ncmpvm, iinst,&
                numpt, numord, inst, crit, prec,&
                nomgd, ncmprf, jnocmp, chamns, codret)
!
!====
! 3. TRANSFORMATION DU CHAM_NO_S EN CHAM_NO :
!====
!
    chamn = chanom
!
    call cnscno(chamns, ' ', 'NON', 'G', chamn,&
                'F', ibid)
!
    call detrsd('CHAM_NO_S', chamns)
!
!====
! 4. BILAN
!====
!
    if (codret .ne. 0) then
        call utmess('A', 'MED_55', sk=chamn)
    endif
    call jedetr('&&LRCNME.NOMCMP_K8')
    call jedema()
!
end subroutine
