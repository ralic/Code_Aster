subroutine verico(nbmato, nbpart, val)
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
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:
!       - VERIFICATION DE LA CONNEXITE DES SOUS-DOMAINES
!
!    - IN :     NBMATO : NOMBRE DE MAILLES
!               NBPART : NOMBRE DE PARTITION
!               CO     : CONNECTIVITE DES MAILLES
!               IDCO   : INDEX DE CO
!               RENUM2 : RENUMEROTATION
!               RENUM3 : RENUMEROTATION INVERSE
!               NUMSDM : SOUS DOMAINES DE CHAQUES MAILLE
!
!    - IN & OUT : VAL : VAUT 1 SI NON CONNEXITE
!----------------------------------------------------------------------
! person_in_charge: aimery.assire at edf.fr
!
! CORPS DU PROGRAMME
    implicit none
!
!
!
! DECLARATION VARIABLES D'APPEL
#include "jeveux.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mess.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
    integer :: nbmato, nbpart, co, idco, renum2, renum3, numsdm, val
!
! DECLARATION VARIABLES LOCALES
    integer :: flagma, flagsd, liste, ima, id, id2, i, nbre, ifm, niv, mail
    real(kind=8) :: tmps(6)
!
! CORPS DU PROGRAMME
    call jemarq()
    call infniv(ifm, niv)
!
    call jeveuo('&&FETSKP.RENUM2', 'L', renum2)
    call jeveuo('&&FETSKP.RENUM3', 'L', renum3)
    call jeveuo('&&FETSKP.CO', 'L', co)
    call jeveuo('&&FETSKP.IDCO', 'L', idco)
    call jeveuo('&&FETSKP.NUMSDM', 'E', numsdm)
!
    if (niv .ge. 2) then
        call uttcpu('CPU.VERICO', 'INIT', ' ')
        call uttcpu('CPU.VERICO', 'DEBUT', ' ')
    endif
!
    call wkvect('&&FETSKP.FLAGMA', 'V V I', nbmato, flagma)
    call wkvect('&&FETSKP.FLAGSD', 'V V I', nbmato, flagsd)
    call wkvect('&&FETSKP.LISTE', 'V V I', nbmato, liste)
!
    do 1 ima = 1, nbmato
!
        mail=zi(renum2-1+ima)
        if (zi(flagma-1+ima) .eq. 0) then
            if (zi(flagsd+zi(numsdm-1+mail)) .eq. 0) then
                zi(flagma-1+ima)=1
                zi(flagsd+zi(numsdm-1+mail))=1
                zi(liste)= ima
                id=0
                id2=0
 2              continue
                do 3 i = zi4(idco-1+zi(liste+id)), zi4(idco-1+zi(liste+ id)+1)-1
                    if (zi(flagma-1+zi4(co-1+i)) .eq. 0) then
                        if (zi(numsdm-1+zi(renum2-1+zi4(co-1+i))) .eq. zi(numsdm-1+mail)) then
                            id2=id2+1
                            zi(liste+id2)=zi4(co-1+i)
                            zi(flagma-1+zi4(co-1+i))=1
                        endif
                    endif
 3              continue
                id=id+1
                if (id .le. id2) goto 2
!
            else
                call u2mess('A', 'UTILITAI5_60')
                val=1
                nbpart=nbpart+1
                zi(flagma-1+ima)=1
                nbre=zi(numsdm-1+mail)
                zi(numsdm-1+mail)=nbpart-1
                zi(liste)= ima
                id=0
                id2=0
 4              continue
                do 5 i = zi4(idco-1+zi(liste+id)), zi4(idco-1+zi(liste+ id)+1)-1
                    if (zi(flagma-1+zi4(co-1+i)) .eq. 0) then
                        if (zi(numsdm-1+zi(renum2-1+zi4(co-1+i))) .eq. nbre) then
                            zi(numsdm-1+zi(renum2-1+zi4(co-1+i)))=&
                            nbpart-1
                            id2=id2+1
                            zi(liste+id2)=zi4(co-1+i)
                            zi(flagma-1+zi4(co-1+i))=1
                        endif
                    endif
 5              continue
                id=id+1
                if (id .le. id2) goto 4
!
            endif
        endif
!
 1  end do
!
    call jedetr('&&FETSKP.LISTE')
    call jedetr('&&FETSKP.FLAGMA')
    call jedetr('&&FETSKP.FLAGSD')
!
    if (niv .ge. 2) then
        call uttcpu('CPU.VERICO', 'FIN', ' ')
        call uttcpr('CPU.VERICO', 6, tmps)
        write(ifm,*)'--- VERIFICATION DE LA CONNEXITE :',tmps(3)
        write(ifm,*)'  '
    endif
!
    call jedema()
end subroutine
