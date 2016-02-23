subroutine cnvois(mail  , lima  , nbma  , idrfma, idmxma,&
                  cninv , jtypma, conect)
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
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jecrec.h"   
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/jeecra.h"
#include "asterfort/jecroc.h"
#include "asterfort/gtvois.h"
#include "asterfort/utlisi.h"
#include "asterfort/assert.h"
!
    character(len=8), intent(in) :: mail
    character(len=24), intent(in) :: cninv
    integer, intent(in) :: nbma
    integer, intent(in) :: lima(nbma)
    integer, intent(in) :: idrfma
    integer, intent(in) :: idmxma
    integer, intent(in) :: jtypma
    character(len=24), intent(in) :: conect


! ------------------------------------------------------------------------------------------------
!        CONNECTIVITE DES MAILLES VOISINES (Methode contact lac)
!                  (POUR LE GROUPE DE MAILLE LIMA)
! ------------------------------------------------------------------------------------------------

! ------------------------------------------------------------------------------------------------
    integer :: ima, ivois, aux(1) ,ntrou, lmail(1)
    integer :: indice, macou, jconec
    integer :: voisin(4), nbvois, nbstock
    character(len=8) :: typma
!    
! --- Initialisation -----------------------------------------------------------------------------
!
    call jemarq()
!
! --- Initalisation de la connectivite -----------------------------------------------------------
!
    nbstock = 0
    do ima=1, nbma
        macou =lima(ima)
        if (zi(jtypma-1+macou) .eq. 2) then
            nbstock = nbstock + 2
        elseif (zi(jtypma-1+macou) .eq. 4) then
            nbstock = nbstock + 2
        elseif (zi(jtypma-1+macou) .eq. 7) then
            nbstock = nbstock + 3
        elseif (zi(jtypma-1+macou) .eq. 9) then
            nbstock = nbstock + 3
        elseif (zi(jtypma-1+macou) .eq. 12) then 
            nbstock = nbstock +4
        elseif (zi(jtypma-1+macou) .eq. 14) then
            nbstock =nbstock + 4
        elseif (zi(jtypma-1+macou) .eq. 16) then
            nbstock = nbstock +4
        else
            ASSERT(.false.)
        end if
    end do    
    call jecrec(conect,'V V I', 'NU', 'CONTIG', 'VARIABLE', idmxma+1-idrfma)
    call jeecra(conect, 'LONT', nbstock+(idmxma+1-idrfma-nbma))
    do ima=1, idmxma+1-idrfma
        macou= ima-1+idrfma
        ntrou = 0
        lmail(1)= macou
        call utlisi('INTER', lmail, 1, lima, nbma, aux, 1, ntrou)
        if (ntrou .eq. 1) then
            if (zi(jtypma-1+macou) .eq. 2) then
                nbvois = 2
            elseif (zi(jtypma-1+macou) .eq. 4) then
                nbvois = 2
            elseif (zi(jtypma-1+macou) .eq. 7) then
                nbvois = 3
            elseif (zi(jtypma-1+macou) .eq. 9) then
                nbvois = 3
            elseif (zi(jtypma-1+macou) .eq. 12) then 
                nbvois = 4
            elseif (zi(jtypma-1+macou) .eq. 14) then
                nbvois = 4
            elseif (zi(jtypma-1+macou) .eq. 16) then
                nbvois = 4 
            else
                ASSERT(.false.)
            end if
            call jecroc(jexnum(conect,ima))
            call jeecra(jexnum(conect,ima), 'LONMAX', ival=nbvois)
        elseif (ntrou .eq. 0) then
            call jecroc(jexnum(conect,ima))
            call jeecra(jexnum(conect,ima), 'LONMAX', ival=1)
        else
            ASSERT(.false.)      
        end if
    end do
!
! --- Remplissage --------------------------------------------------------------------------------
! 
    do ima=1, nbma
        macou =lima(ima)
        if (zi(jtypma-1+macou) .eq. 2) then
            nbvois = 2
            typma = 'SE2'
        elseif (zi(jtypma-1+macou) .eq. 4) then
            nbvois = 2
            typma = 'SE3'
        elseif (zi(jtypma-1+macou) .eq. 7) then
            nbvois = 3
            typma = 'TR3'
        elseif (zi(jtypma-1+macou) .eq. 9) then
            nbvois = 3
            typma = 'TR6'
        elseif (zi(jtypma-1+macou) .eq. 12) then 
            nbvois = 4
            typma = 'QU4'
        elseif (zi(jtypma-1+macou) .eq. 14) then
            nbvois = 4
            typma = 'QU8'
        elseif (zi(jtypma-1+macou) .eq. 16) then
            nbvois = 4
            typma = 'QU9'
        else
            ASSERT(.false.)
        end if
        indice = macou+1-idrfma
        call jeveuo(jexnum(conect,indice), 'E', jconec)
        call gtvois(mail  , lima  , nbma  , macou, typma,&
                    cninv , nbvois, voisin)
        do ivois=1, nbvois
            zi(jconec+ivois-1)=voisin(ivois) 
        end do
    end do
 ! -----------------------------------------------------------------------------------------------
    call jedema()
end subroutine
