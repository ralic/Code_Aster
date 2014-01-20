subroutine rsutc4(resu, motfac, iocc, dimlis, lisch,&
                  nbch, acceno)
    implicit none
#include "jeveux.h"
#include "asterfort/getvtx.h"
#include "asterfort/indk16.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: iocc, dimlis, nbch
    logical :: acceno
    character(len=*) :: resu, lisch(*), motfac
!
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
!----------------------------------------------------------------------
!
!     TRAITER LES MOTS CLE :
!        /  TOUT_CHAM (='OUI' PAR DEFAUT)
!        /  NOM_CHAM = (NOCH1,NOCH2,...)
!     ET ETABLIR LA LISTE DES NOMS SYMBOLIQUES A TRAITER.
!
! IN  : RESU    : K19  : SD_RESULTAT
! IN  : MOTFAC  : K16  : NOM DU MOT CLE FACTEUR (OU ' ')
! IN  : IOCC    : I    : NUMERO D'OCCURRENCE DE MOTFAC (OU 1)
! IN  : DIMLIS  : I    : LONGUEUR DE LISCH
! OUT : LISCH   : L_K16: LISTE DES NOMS TROUVES
! OUT : NBCH    : I    : NOMBRE DE CHAMPS TROUVES (OU -NBCH SI
!                        SI LISCH EST TROP COURTE)
! OUT : ACCENO  : L : .TRUE. : L'UTILISATEUR A UTILISE NOM_CHAM
!                     .FALSE. : L'UTILISATEUR N'A PAS UTILISE NOM_CHAM
!                               (=> TOUT_CHAM PAR DEFAUT)
!
!----------------------------------------------------------------------
!
!
    character(len=19) :: resu2
    integer :: nbnosy,  isy, n2,  ibid, k, kk
    character(len=16), pointer :: lich(:) => null()
    character(len=16), pointer :: litou(:) => null()
!
    resu2 = resu
!
!     --- ON REGARDE LA LISTE DES CHAMPS POSSIBLES POUR RESU:
    call jelira(resu2//'.DESC', 'NOMUTI', nbnosy)
    AS_ALLOCATE(vk16=litou, size=nbnosy)
    do 10 isy = 1, nbnosy
        call jenuno(jexnum(resu2//'.DESC', isy), litou(isy))
10  end do
!
    acceno = .false.
!
    call getvtx(motfac, 'NOM_CHAM', iocc=iocc, nbval=0, nbret=n2)
    n2 = -n2
    if (n2 .gt. 0) then
        AS_ALLOCATE(vk16=lich, size=n2)
        call getvtx(motfac, 'NOM_CHAM', iocc=iocc, nbval=n2, vect=lich,&
                    nbret=ibid)
        do 20,k = 1,n2
        kk = indk16(litou,lich(k),1,nbnosy)
        if (kk .eq. 0) then
            call utmess('F', 'PREPOST4_77', sk=lich(k))
        endif
20      continue
        nbch = n2
        do 30,k = 1,min(nbch,dimlis)
        lisch(k) = lich(k)
30      continue
        acceno = .true.
!
    else
        nbch = nbnosy
        do 40,k = 1,min(nbch,dimlis)
        lisch(k) = litou(k)
40      continue
    endif
!
    if (nbch .gt. dimlis) nbch = -nbch
!
    AS_DEALLOCATE(vk16=litou)
    AS_DEALLOCATE(vk16=lich)
!
end subroutine
