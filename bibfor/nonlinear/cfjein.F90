subroutine cfjein(noma, defico, resoco, depdel)
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
    implicit none
#include "jeveux.h"
#include "asterfort/caladu.h"
#include "asterfort/cfdisd.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfdisr.h"
#include "asterfort/cfimp2.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    character(len=8) :: noma
    character(len=24) :: defico, resoco
    character(len=19) :: depdel
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE - ALGORITHME)
!
! CALCUL DES JEUX INITIAUX AJEU+ = AJEU/I/N - A.DDEPLA
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  DEPDEL : INCREMENT DE DEPLACEMENT CUMULE DEPUIS DEBUT DU PAS
!
!
!
!
    integer :: ifm, niv
    integer :: nbddl, jdecal
    integer :: iliai, ialarm
    character(len=19) :: ddepl0
    integer :: jddep0, jdepde
    character(len=24) :: jeuite, jeux
    integer :: jjeuit, jjeux
    character(len=24) :: apddl, apcoef, appoin
    integer :: japddl, japcoe, japptr
    character(len=24) :: apcofr
    integer :: japcof
    character(len=24) :: clreac
    integer :: jclrea
    logical :: reapre
    integer :: nbliai, neq, ndimg, nesmax
    logical :: lgliss, lctfd, llagrf
    real(kind=8) :: aljeu
    real(kind=8) :: jeuold, jeuini, jexini, jeyini
    real(kind=8) :: val1, val2, val
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ......... CALCUL DES JEUX INITIAUX'
    endif
!
! --- PARAMETRES
!
    ndimg = cfdisd(resoco,'NDIM' )
    nbliai = cfdisd(resoco,'NBLIAI')
    neq = cfdisd(resoco,'NEQ' )
    nesmax = cfdisd(resoco,'NESMAX')
    lctfd = cfdisl(defico,'FROT_DISCRET')
    llagrf = cfdisl(defico,'FROT_LAGR' )
    lgliss = cfdisl(defico,'CONT_DISC_GLIS')
    aljeu = cfdisr(defico,'ALARME_JEU' )
!
! --- INITIALISATIONS
!
    ialarm = 0
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    appoin = resoco(1:14)//'.APPOIN'
    apddl = resoco(1:14)//'.APDDL'
    apcoef = resoco(1:14)//'.APCOEF'
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(apcoef, 'L', japcoe)
!
    jeuite = resoco(1:14)//'.JEUITE'
    jeux = resoco(1:14)//'.JEUX'
    call jeveuo(jeuite, 'E', jjeuit)
    call jeveuo(jeux, 'E', jjeux)
!
    if (lctfd) then
        apcofr = resoco(1:14)//'.APCOFR'
        call jeveuo(apcofr, 'L', japcof)
    endif
!
    clreac = resoco(1:14)//'.REAL'
    call jeveuo(clreac, 'L', jclrea)
!
! --- PARAMETRES DE REACTUALISATION
!
    reapre = zl(jclrea+3-1)
!
! --- ACCES AUX CHAMPS DE TRAVAIL
! --- DDEPL0: INCREMENT DE SOLUTION SANS CORRECTION DU CONTACT
!
    ddepl0 = resoco(1:14)//'.DEL0'
    call jeveuo(ddepl0(1:19)//'.VALE', 'L', jddep0)
!
! --- INCREMENT DE DEPLACEMENT DEPUIS LE DEBUT DU PAS DE TEMPS
!
    call jeveuo(depdel(1:19)//'.VALE', 'L', jdepde)
!
! --- ON CALCULE LE NOUVEAU JEU : AJEU+ = AJEU/I/N - A.DDEPLA
!
    do 10 iliai = 1, nbliai
!
! ----- ACCES TABLEAU LIAISONS
!
        jdecal = zi(japptr+iliai-1)
        nbddl = zi(japptr+iliai) - zi(japptr+iliai-1)
!
! ----- INCR. DE JEU SANS CORRECTION [A].{DDEPL0}
!
        call caladu(neq, nbddl, zr(japcoe+jdecal), zi(japddl+jdecal), zr(jddep0),&
                    val)
!
! ----- JEU AVANT L'ITERATION DE NEWTON {JEU(DEPTOT)}
!
        jeuold = zr(jjeuit+3*(iliai-1)+1-1)
!
! ----- JEU SANS CORRECTION DU CONTACT: {JEU(DEPTOT)} - [A].{DDEPL0}
!
        jeuini = jeuold - val
!
! ----- SAUVEGARDES
!
        zr(jjeux+3*(iliai-1)+1-1) = jeuini
!
! ----- JEUX TANGENTS
!
        if (lctfd) then
!
! ------- INCR. JEUX TANGENTS SANS CORRECTION
!
            call caladu(neq, nbddl, zr(japcof+jdecal), zi(japddl+jdecal), zr(jddep0),&
                        val1)
!
! ------- INCR. JEUX TANGENTS DEPUIS LE DEBUT DU PAS DE TEMPS
!
            call caladu(neq, nbddl, zr(japcof+jdecal), zi(japddl+jdecal), zr(jdepde),&
                        val2)
!
! ------- INCR. DE JEU DEPUIS LE DEBUT DU PAS DE TEMPS SANS CORR.
!
            jexini = val1 + val2
!
            if ((ndimg.eq.2) .and. llagrf .and. reapre) then
                zr(jjeuit+3*(iliai-1)+2-1) = 0.d0
            endif
!
            zr(jjeux+3*(iliai-1)+2-1) = jexini
!
            if (ndimg .eq. 3) then
!
! --------- INCR. DE JEU SANS CORRECTION
!
                call caladu(neq, nbddl, zr(japcof+jdecal+30*nesmax), zi( japddl+jdecal),&
                            zr(jddep0), val1)
!
! --------- INCR. DE JEU DEPUIS LE DEBUT DU PAS DE TEMPS
!
                call caladu(neq, nbddl, zr(japcof+jdecal+30*nesmax), zi( japddl+jdecal),&
                            zr(jdepde), val2)
!
! --------- INCR. DE JEU DEPUIS LE DEBUT DU PAS DE TEMPS SANS CORR.
!
                jeyini = val1 + val2
                zr(jjeux+3*(iliai-1)+3-1) = jeyini
            endif
        endif
!
10  end do
!
! --- ALARME SI DECOLLEMENT ALORS QUE GLISSIERE
!
    do 15 iliai = 1, nbliai
        if (lgliss) then
            jeuini = zr(jjeux+3*(iliai-1)+1-1)
            if (jeuini .gt. aljeu) then
                ialarm = ialarm+1
                if (ialarm .eq. 1) then
                    call utmess('A', 'CONTACT_9')
                endif
                call cfimp2(defico, resoco, noma, iliai, 'C0',&
                            'ALJ')
            endif
        endif
15  end do
!
    call jedema()
!
end subroutine
