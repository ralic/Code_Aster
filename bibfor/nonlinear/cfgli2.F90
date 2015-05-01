subroutine cfgli2(noma, defico, resoco, neq, nbliai,&
                  nbliac, llf, ajliai, spliai, indic,&
                  liasup)
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
!
#include "asterc/r8miem.h"
#include "asterfort/assert.h"
#include "asterfort/caladu.h"
#include "asterfort/cfelpv.h"
#include "asterfort/cfimp2.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/cftabl.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: resoco, defico
    character(len=8) :: noma
    integer :: indic
    integer :: ajliai, spliai, nbliai, neq
    integer :: nbliac, llf
    aster_logical :: liasup
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! VERIFICATION QUE LES LIAISONS SONT BIEN GLISSANTES - VERSION 2D
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
    character(len=19) :: liac, typl, mu
    integer :: jliac, jtypl, jmu
    character(len=24) :: tacfin, appoin, apcofr, apddl
    integer :: jtacf, japptr, japcof, japddl
    character(len=19) :: ddeplc, ddepl0, ddelt
    character(len=24) :: jeuite
    integer :: jjeuit
    integer :: llf1, llf2, btotal, lfmin
    integer :: iliai, iliac, iliac2
    integer :: jdecal, nbddl, posit
    real(kind=8) :: coefff, xpdt, xcos
    real(kind=8) :: jexinc, jexold, jexnew, val1, val2
    character(len=1) :: typeaj
    character(len=2) :: typlia, typec0, typef0
    integer :: ztacf
    aster_logical :: lelpiv
    real(kind=8), pointer :: vddelt(:) => null()
    real(kind=8), pointer :: ddep0(:) => null()
    real(kind=8), pointer :: ddepc(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    lfmin = 0
    indic = 0
    llf1 = 0
    llf2 = 0
    typeaj = 'A'
    typec0 = 'C0'
    typef0 = 'F0'
    btotal = nbliac + llf + llf1 + llf2
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    appoin = resoco(1:14)//'.APPOIN'
    apcofr = resoco(1:14)//'.APCOFR'
    apddl = resoco(1:14)//'.APDDL'
    liac = resoco(1:14)//'.LIAC'
    typl = resoco(1:14)//'.TYPL'
    tacfin = resoco(1:14)//'.TACFIN'
    jeuite = resoco(1:14)//'.JEUITE'
    mu = resoco(1:14)//'.MU'
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(apcofr, 'L', japcof)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(liac, 'L', jliac)
    call jeveuo(typl, 'L', jtypl)
    call jeveuo(tacfin, 'L', jtacf)
    call jeveuo(jeuite, 'L', jjeuit)
    call jeveuo(mu, 'E', jmu)
    ztacf = cfmmvd('ZTACF')
!
! --- ACCES AUX CHAMPS DE TRAVAIL
! --- DDEPL0: INCREMENT DE SOLUTION SANS CORRECTION DU CONTACT
! --- DDEPLC: INCREMENT DE SOLUTION APRES CORRECTION DU CONTACT
! --- DDELT : INCREMENT DE SOLUTION ITERATION DE CONTACT
!
    ddepl0 = resoco(1:14)//'.DEL0'
    ddeplc = resoco(1:14)//'.DELC'
    ddelt = resoco(1:14)//'.DDEL'
    call jeveuo(ddepl0(1:19)//'.VALE', 'L', vr=ddep0)
    call jeveuo(ddeplc(1:19)//'.VALE', 'L', vr=ddepc)
    call jeveuo(ddelt (1:19)//'.VALE', 'L', vr=vddelt)
!
! --- LIAISON GLISSANTE ?
!
    do 710 iliac = 1, btotal
        iliai = zi(jliac+iliac-1)
        typlia = zk8(jtypl+iliac-1)(1:2)
        if (typlia .eq. typec0) then
            do 720 iliac2 = iliac+1, btotal
                if (zi(jliac-1+iliac2) .eq. iliai) goto 710
720         continue
!
! ------- ON NE PREND PAS EN COMPTE UNE LIAISON A PIVOT NUL
!
            call cfelpv(iliai, typef0, resoco, nbliai, lelpiv)
            if (lelpiv) goto 710
!
            coefff = zr(jtacf+ztacf*(iliai-1)+0)
            jdecal = zi(japptr+iliai-1)
            nbddl = zi(japptr+iliai) - zi(japptr+iliai-1)
!
! ------- JEU TANGENT AVANT L'ITERATION DE NEWTON
!
            jexold = zr(jjeuit+3*(iliai-1)+2-1)
!
! ------- CALCUL DU JEU TANGENT TOTAL
!
            call caladu(neq, nbddl, zr(japcof+jdecal), zi(japddl+jdecal), vddelt,&
                        val1)
            call caladu(neq, nbddl, zr(japcof+jdecal), zi(japddl+jdecal), ddepc,&
                        val2)
            jexnew = jexold + val1 + val2
            xpdt = zr(jmu+3*nbliai+iliai-1)*jexnew
            if (xpdt .lt. 0.d0) then
!
! --------- LA LIAISON EST EN FAITE ADHERENTE
!
                posit = nbliac + llf + 1
                call cftabl(indic, nbliac, ajliai, spliai, llf,&
                            llf1, llf2, resoco, typeaj, posit,&
                            iliai, typef0)
                call cfimp2(defico, resoco, noma, iliai, 'F3',&
                            'ADH')
                lfmin = lfmin + 1
                zr(jmu+3*nbliai+iliai-1) = 0.d0
            else if (xpdt.eq.0.d0) then
!
! --------- LA LIAISON EST BIEN GLISSANTE
! --------- ON MET A JOUR MU_G
!
                call caladu(neq, nbddl, zr(japcof+jdecal), zi(japddl+ jdecal), ddep0,&
                            jexinc)
                jexnew = jexold + jexinc
                if (abs(jexnew) .le. r8miem()) then
                    zr(jmu+3*nbliai+iliai-1) = 0.d0
                else
                    xcos = jexnew / abs(jexnew)
                    zr(jmu+3*nbliai+iliai-1) = coefff * xcos
                endif
            endif
        endif
710 end do
!
    if (lfmin .gt. 0) then
        ASSERT(indic.eq.1)
    endif
!
    liasup = lfmin.ne.0
!
    call jedema()
!
end subroutine
