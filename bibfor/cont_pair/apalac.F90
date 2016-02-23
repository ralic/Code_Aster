subroutine apalac(lnewtg, mail, defico, resoco, tole, loptin)
   
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/jecrec.h"
#include "asterfort/jeveuo.h"
#include "asterfort/assert.h"
#include "asterfort/jemarq.h"
#include "asterfort/jedema.h"
#include "asterfort/gtlima.h"
#include "asterfort/aplcpb.h"
#include "asterfort/aplcpg.h"
#include "asterfort/aplcfb.h"
#include "asterfort/apsave.h"
#include "asterfort/jedetr.h"
#include "asterfort/cfdisi.h"
#include "asterfort/jelira.h"
#include "asterfort/wkvect.h"
#include "asterfort/jerazo.h"
#include "asterf_types.h"

!
    character(len=24) ::resoco, defico
    character(len=8) :: mail
    real(kind=8) :: tole
    aster_logical :: lnewtg, loptin
! ----------------------------------------------------------------------
!     RECUPERATION DES LISTES DE MAILLE ASSOCI A UNE ZONE DE CONTACT
! ----------------------------------------------------------------------
!   IN        MAIL       MAILLAGE
!   IN        DEFICO     SD DEFINITION DU CONTACT
!   IN/OUT    RESOCO     SD RESOLUTION DU CONTACT
!   IN        TOLE       TOLERANCE D'APPARIEMENT   
! ----------------------------------------------------------------------
!
    integer :: izone, nzoco, nbmma, nbmes, nbpatch
    integer :: nmactt, jlimama, jlimaes, jcrnud
    character(len=24) ::limama, limaes, gapmoy, crnudd, pair_method
    integer, pointer :: vectap(:) => null()
!
!
    call jemarq()
! --- Initialisation    ------------------------------------------------
!
    nzoco = cfdisi(defico,'NZOCO')
    limama = '&&APALAC.LIMAMA' 
    limaes = '&&APALAC.LIMAES'
    gapmoy = resoco(1:14)//'.GAPINT'
    crnudd = resoco(1:14)//'.NUDD'
    nmactt=0
    pair_method = 'PANG_ROBUSTE'
! --- INDICATEUR DE NEW APPARIEMENT ------------------------------------
    call jeveuo(crnudd, 'E', jcrnud)
    zl(jcrnud)= .true.
    call jelira(gapmoy, 'LONUTI', nbpatch)
    call jerazo(gapmoy, nbpatch, 1)
!
! --- BOUCLE SUR LES ZONES
!
    do izone = 1, nzoco
! ------ On recupère les listes de mailles de la zone ------------------
! 
        call gtlima(defico, izone, nbmma, nbmes, limama, limaes)
! ------ Appariement sur la zone izone (force brute) -------------------
!       
        call jeveuo(limama,'L', jlimama)
        call jeveuo(limaes,'L', jlimaes)
        if (pair_method.eq.'PANG_ROBUSTE') then
            call aplcpb(mail, zi(jlimama), nbmma, zi(jlimaes), nbmes, resoco,&
                        izone, lnewtg, tole, nmactt, vectap, loptin)
        elseif (pair_method.eq.'PANG') then
            call aplcpg(mail, zi(jlimama), nbmma, zi(jlimaes), nbmes, resoco,&
                        izone, lnewtg, tole, nmactt, vectap, loptin)
        elseif (pair_method.eq.'FORCE_BRUTE') then
            call aplcfb(mail, zi(jlimama), nbmma, zi(jlimaes), nbmes, resoco,&
                        izone, lnewtg, tole, nmactt, vectap)
        else
            ASSERT(.false.)
        endif
! ------ Netoyage des objets jeveux tmp ---------------------------------
!
        call jedetr(limama)
        call jedetr(limaes)

    end do
! ------ Sauvegarde de l'appariiement LAC ------------------------------
!
    call apsave(vectap, resoco, nmactt)
!

    call jedema()

end subroutine        
