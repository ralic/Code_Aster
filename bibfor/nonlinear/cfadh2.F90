subroutine cfadh2(resoco, defico, noma, indic, nbliac,&
                  nbliai, ajliai, spliai, llf)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfimp2.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/cftabl.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    character(len=24) :: resoco, defico
    character(len=8) :: noma
    integer :: indic
    integer :: ajliai, spliai, nbliai
    integer :: nbliac, llf
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! VERIFICATION QUE LES LIAISONS SONT BIEN ADHERENTES - VERSION 2D
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NOMA   : NOM DU MAILLAGE
! OUT INDIC  :+1 ON A RAJOUTE UNE LIAISON
!             -1 ON A ENLEVE UNE LIAISON
! IN  NBLIAI : NOMBRE DE LIAISONS DE CONTACT
! I/O NBLIAC : NOMBRE DE LIAISONS ACTIVES
! I/O AJLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON CORRECTE DU CALCUL
!              DE LA MATRICE DE CONTACT ACM1AT
! I/O SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR CM1A
! I/O LLF    : NOMBRE DE LIAISONS DE FROTTEMENT
!
!
!
!
    integer :: btotal, llf1, llf2
    integer :: iliai, iliai2, iliac, iliac2
    integer :: icompt
    integer :: compt0, compts, comptn
    character(len=24) :: splf0
    integer :: jsplf0
    real(kind=8) :: coefff, lambdf, lambdc, xquot
    character(len=1) :: typesp
    character(len=2) :: typlia, typli2, typec0, typef0
    character(len=19) :: liac, typl, mu
    integer :: jliac, jtypl, jmu
    character(len=24) :: tacfin
    integer :: jtacf
    integer :: ztacf
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    typesp = 'S'
    typec0 = 'C0'
    typef0 = 'F0'
    compt0 = 0
    compts = 0
    comptn = 0
    llf1 = 0
    llf2 = 0
    splf0 = '&&CFADH2.SUPLF0'
    btotal = nbliac + llf + llf1 + llf2
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    liac = resoco(1:14)//'.LIAC'
    typl = resoco(1:14)//'.TYPL'
    mu = resoco(1:14)//'.MU'
    tacfin = resoco(1:14)//'.TACFIN'
    call jeveuo(liac, 'L', jliac)
    call jeveuo(typl, 'L', jtypl)
    call jeveuo(mu, 'E', jmu)
    call jeveuo(tacfin, 'L', jtacf)
    ztacf = cfmmvd('ZTACF')
!
! --- VECTEUR POUR STOCKER LES LIAISONS (GLISSANTES) A SUPPRIMER
!
    call wkvect(splf0, 'V V I', llf, jsplf0)
!
! --- DETECTION DES LIAISONS EVENTUELLEMENT GLISSANTES
!
    do 300 iliac = 1, btotal
        comptn = 0
        iliai = zi(jliac-1+iliac)
        typlia = zk8(jtypl-1+iliac)(1:2)
!
! ----- CAS D'UNE LIAISON DE FROTTEMENT
!
        if (typlia .eq. typef0) then
            compt0 = compt0 + 1
            coefff = zr(jtacf+ztacf*(iliai-1)+0)
            lambdf = zr(jmu+nbliac+compt0-1)
!
! ------- DECALAGE DE LA LIAISON DE CONTACT ASSOCIEE
!
            do 310 iliac2 = 1, iliac - 1
                typli2 = zk8(jtypl+iliac2-1)(1:2)
                if (typli2 .eq. typec0) then
                    comptn = comptn + 1
                    iliai2 = zi(jliac+iliac2-1)
                    if (iliai .eq. iliai2) then
                        goto 312
                    endif
                endif
310          continue
            ASSERT(.false.)
        endif
        goto 300
312      continue
!
! ----- PRESSION DE CONTACT
!
        lambdc = zr(jmu+comptn-1)
        if (lambdc .gt. 0.d0) then
            xquot = abs(lambdf)/lambdc
        else
            xquot = 0.d0
        endif
!
! ----- LA LIAISON EST GLISSANTE ?
!
        if (abs(xquot) .ge. coefff) then
!
! ------- OUI
!
            compts = compts + 1
            zi(jsplf0-1+compts) = iliac
            zr(jmu+3*nbliai+iliai-1) = (lambdf/abs(lambdf)) * coefff
        else
!
! ------- NON
!
            zr(jmu+nbliac+compt0-compts-1) = lambdf
            zr(jmu+3*nbliai+iliai-1) = 0.d0
        endif
300  end do
!
! --- SUPPRESSION DES LIAISONS QUI SONT EN FAIT GLISSANTES
!
    do 160 icompt = 1, compts
        iliac = zi(jsplf0+compts-icompt)
        iliai = zi(jliac-1+iliac)
        call cftabl(indic, nbliac, ajliai, spliai, llf,&
                    llf1, llf2, resoco, typesp, iliac,&
                    iliai, typef0)
        call cfimp2(defico, resoco, noma, iliai, 'F3',&
                    'GLI')
160  end do
!
    call jedetr(splf0)
    call jedema()
!
end subroutine
