subroutine cfgli3(noma, defico, resoco, neq, nesmax,&
                  nbliai, nbliac, llf, llf1, llf2,&
                  ajliai, spliai, indic, xmul, liasup)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/caladu.h"
#include "asterfort/cfelpv.h"
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
    integer :: ajliai, spliai, nbliai, neq, nesmax
    integer :: nbliac, llf, llf1, llf2
    real(kind=8) :: xmul
    aster_logical :: liasup
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! VERIFICATION QUE LES LIAISONS SONT BIEN GLISSANTES - VERSION 3D
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NOMA   : NOM DU MAILLAGE
! IN  NEQ    : NOMBRE D'EQUATIONS
! IN  NBLIAI : NOMBRE DE LIAISONS DE CONTACT
! I/O NBLIAC : NOMBRE DE LIAISONS ACTIVES
! I/O AJLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON CORRECTE DU CALCUL
!              DE LA MATRICE DE CONTACT ACM1AT
! I/O SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR CM1A
! I/O LLF    : NOMBRE DE LIAISONS DE FROTTEMENT
! OUT INDIC  : +1 ON A RAJOUTE UNE LIAISON
!              -1 ON A ENLEVE UNE LIAISON
! OUT LIASUP : .TRUE. SI AU MOINS UNE LIAISON SUPPRIMEE
!
!
!
!
    character(len=24) :: nmgli1, nmgli2, nmadhr
    integer :: jgli1, jgli2, jadhr
    real(kind=8) :: lambdc, lambdf, xval
    character(len=1) :: typeaj
    character(len=2) :: typli1
    character(len=2) :: typec0, typef0, typef1, typef2
    integer :: iliai1, iliai2, iliac1, iliac2
    integer :: iliai, iliac
    character(len=19) :: liac, typl, mu, copo
    integer :: jliac, jtypl, jmu, jcopo
    character(len=24) :: tacfin, appoin, apcofr, apddl
    integer :: jtacf, japptr, japcof, japddl
    integer :: ztacf
    integer :: lfmin, lfmin1, lfmin2
    aster_logical :: lelpiv, lelpi1, lelpi2, trouac
    integer :: nbddl, jdecal, btotal, posit
    integer :: compt0
    real(kind=8) :: ajeufx, ajeufy, glis, coefff
    character(len=19) :: deplc
    real(kind=8), pointer :: vale(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    lfmin = 0
    lfmin1 = 0
    lfmin2 = 0
    compt0 = 0
    typeaj = 'A'
    btotal = nbliac + llf + llf1 + llf2
    typec0 = 'C0'
    typef0 = 'F0'
    typef1 = 'F1'
    typef2 = 'F2'
    liasup = .false.
!
! --- PAS DE LIAISONS GLISSANTES -> ON SORT
!
    if ((llf+llf1+llf2) .ge. nbliac) then
        goto 999
    endif
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    appoin = resoco(1:14)//'.APPOIN'
    apcofr = resoco(1:14)//'.APCOFR'
    apddl = resoco(1:14)//'.APDDL'
    liac = resoco(1:14)//'.LIAC'
    typl = resoco(1:14)//'.TYPL'
    tacfin = resoco(1:14)//'.TACFIN'
    mu = resoco(1:14)//'.MU'
    copo = resoco(1:14)//'.COPO'
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(apcofr, 'L', japcof)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(liac, 'L', jliac)
    call jeveuo(typl, 'L', jtypl)
    call jeveuo(tacfin, 'L', jtacf)
    call jeveuo(mu, 'E', jmu)
    call jeveuo(copo, 'E', jcopo)
    ztacf = cfmmvd('ZTACF')
!
! --- ACCES AUX CHAMPS DE TRAVAIL
! --- DEPLC : INCREMENT DE DEPLACEMENT CUMULE DEPUIS DEBUT
! ---         DU PAS DE TEMPS AVEC CORRECTION DU CONTACT
!
    deplc = resoco(1:14)//'.DEPC'
    call jeveuo(deplc (1:19)//'.VALE', 'L', vr=vale)
!
! --- CREATION DES OBJETS DE TRAVAIL
!
    nmgli1 = '&&CFGLI3.GLI1'
    nmgli2 = '&&CFGLI3.GLI2'
    nmadhr = '&&CFGLI3.ADHR'
    call wkvect(nmgli1, 'V V I', nbliac, jgli1)
    call wkvect(nmgli2, 'V V I', nbliac, jgli2)
    call wkvect(nmadhr, 'V V I', nbliac, jadhr)
!
! --- LIAISON GLISSANTE ?
!
    do 100 iliac1 = 1, btotal
        iliai1 = zi(jliac+iliac1-1)
        typli1 = zk8(jtypl+iliac1-1)(1:2)
        if (typli1 .eq. typec0) then
!
            compt0 = compt0 + 1
!
! ------- INFORMATIONS SUR LA LIAISON
!
            coefff = zr(jtacf+ztacf*(iliai1-1)+0)
            jdecal = zi(japptr+iliai1-1)
            nbddl = zi(japptr+iliai1) - zi(japptr+iliai1-1)
            lambdc = zr(jmu+compt0-1)
!
! ------- CALCUL DES JEUX
!
            ajeufx = 0.d0
            ajeufy = 0.d0
            call cfelpv(iliai1, typef1, resoco, nbliai, lelpi1)
            if (.not.lelpi1) then
                call caladu(neq, nbddl, zr(japcof+jdecal), zi(japddl+ jdecal), vale,&
                            ajeufx)
            endif
!
            call cfelpv(iliai1, typef2, resoco, nbliai, lelpi2)
            if (.not.lelpi2) then
                call caladu(neq, nbddl, zr(japcof+jdecal+30*nesmax), zi(japddl+jdecal), vale,&
                            ajeufy)
            endif
            glis = sqrt( ajeufx**2 + ajeufy**2 )
!
! ------- LAGRANGE DE FROTTEMENT
!
            if (lambdc .gt. 0.d0) then
                lambdf = coefff*lambdc
            else
                lambdf = 0.d0
            endif
!
! ------- EST-ELLE ADHERENTE ?
!
            if ((glis.gt.r8prem()) .and. (glis .lt. (lambdf/xmul**2))) then
                trouac = .false.
                do 157 iliac2 = iliac1 + 1, btotal
                    iliai2 = zi(jliac+iliac2-1)
                    if (iliai1 .eq. iliai2) trouac = .true.
157             continue
                if (.not.trouac) then
                    call cfelpv(iliai1, typef0, resoco, nbliai, lelpiv)
                    if (.not.lelpiv) then
                        call cfelpv(iliai1, typef1, resoco, nbliai, lelpi1)
                        if (lelpi1) then
                            lfmin2 = lfmin2 + 1
                            zi(jgli2+lfmin2-1) = iliai1
                            goto 100
                        endif
                        call cfelpv(iliai1, typef2, resoco, nbliai, lelpi1)
                        if (lelpi2) then
                            lfmin1 = lfmin1 + 1
                            zi(jgli1+lfmin1-1) = iliai1
                            goto 100
                        endif
                        lfmin = lfmin + 1
                        zi(jadhr+lfmin-1) = iliai1
                    endif
                endif
            endif
        endif
100 end do
!
! --- AJOUTER LIAISON ADHERENTE
!
    do 197 iliac = 1, lfmin
        iliai = zi(jadhr+iliac-1)
        posit = nbliac + llf + llf1 + llf2 + 1
        call cftabl(indic, nbliac, ajliai, spliai, llf,&
                    llf1, llf2, resoco, typeaj, posit,&
                    iliai, typef0)
        call cfimp2(defico, resoco, noma, iliai, typef0,&
                    'ADH')
197 end do
    do 198 iliac = 1, lfmin1
        iliai = zi(jgli1+iliac-1)
        posit = nbliac + llf + llf1 + llf2 + 1
        call cftabl(indic, nbliac, ajliai, spliai, llf,&
                    llf1, llf2, resoco, typeaj, posit,&
                    iliai, typef1)
        call cfimp2(defico, resoco, noma, iliai, typef1,&
                    'ADH')
198 end do
    do 199 iliac = 1, lfmin2
        iliai = zi(jgli2+iliac-1)
        posit = nbliac + llf + llf1 + llf2 + 1
        call cftabl(indic, nbliac, ajliai, spliai, llf,&
                    llf1, llf2, resoco, typeaj, posit,&
                    iliai, typef2)
        call cfimp2(defico, resoco, noma, iliai, typef2,&
                    'ADH')
199 end do
!
! --- A-TON MODIFIE DES LIAISONS ?
!
    liasup = (lfmin+lfmin1+lfmin2).ne.0
!
! --- MISE A JOUR DU PARAMETRE
!
    if (liasup) then
        xval = xmul**2
        if (xval .lt. (1.d0/r8prem())) then
            xmul = xmul*sqrt(10.d0)
        endif
        zr(jcopo) = xmul
    endif
!
    call jedetr(nmgli1)
    call jedetr(nmgli2)
    call jedetr(nmadhr)
!
999 continue
    call jedema()
!
end subroutine
