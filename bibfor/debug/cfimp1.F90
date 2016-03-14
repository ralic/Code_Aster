subroutine cfimp1(phase, noma, sdcont_defi, sdcont_solv, ifm)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/apinfi.h"
#include "asterfort/apnomp.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisd.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfnoap.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8) :: noma
    character(len=24) :: sdcont_defi, sdcont_solv
    character(len=3) :: phase
    integer :: ifm
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE - APPARIEMENT - UTILITAIRE)
!
! IMPRESSION DES LIAISONS ESCLAVE/MAITRE
!
! ----------------------------------------------------------------------
!
!
! IN  sdcont_defi : SD DE DEFINITION DU CONTACT
! IN  sdcont_solv : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  PHASE  : 'INI' LIAISONS INITIALES
!              'FIN' LIAISONS FINALES
! IN  NOMA   : NOM DU MAILLAGE
! IN  IFM    : UNITE D'IMPRESSION DU MESSAGE
!
!
!
!
    integer :: iliac, iliai, actif, izone, ip
    character(len=8) :: nomapp
    character(len=16) :: nomnoe
    character(len=14) :: chaiac
    character(len=4) :: type2
    real(kind=8) :: jeu
    character(len=24) :: jeuite, jeux
    integer :: jjeuit, jjeux
    character(len=19) :: liac
    integer :: jliac
    character(len=24) :: numlia
    integer :: jnumli
    character(len=19) :: sdappa
    integer :: typapp, entapp
    integer :: nbliai, nbliac
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD CONTACT
!
    liac = sdcont_solv(1:14)//'.LIAC'
    numlia = sdcont_solv(1:14)//'.NUMLIA'
    call jeveuo(liac, 'L', jliac)
    call jeveuo(numlia, 'L', jnumli)
!
    jeuite = sdcont_solv(1:14)//'.JEUITE'
    jeux = sdcont_solv(1:14)//'.JEUX'
    call jeveuo(jeuite, 'L', jjeuit)
    call jeveuo(jeux, 'L', jjeux)
!
! --- SD APPARIEMENT
!
    sdappa = sdcont_solv(1:14)//'.APPA'
!
! --- INFORMATIONS SUR CONTACT
!
    nbliai = cfdisd(sdcont_solv,'NBLIAI')
    nbliac = cfdisd(sdcont_solv,'NBLIAC')
!
! --- AFFICHAGE EN-TETE
!
    write(ifm,10)  nbliai
    if (phase .eq. 'INI') then
        write(ifm,101) nbliac
    else if (phase.eq.'FIN') then
        write(ifm,301) nbliac
    else
        ASSERT(.false.)
    endif
!
    write(ifm,20)
!
! --- BOUCLE SUR LES LIAISONS
!
    do iliai = 1, nbliai
!
! ----- POINT DE CONTACT
!
        ip = zi(jnumli+4*(iliai-1)+1-1)
!
! ----- INFOS APPARIEMENT
!
        call apinfi(sdappa, 'APPARI_TYPE', ip, typapp)
        call apinfi(sdappa, 'APPARI_ENTITE', ip, entapp)
        call apinfi(sdappa, 'APPARI_ZONE', ip, izone)
!
! ----- NOM DU NOEUD ESCLAVE
!
        call apnomp(sdappa, ip, nomnoe)
!
! ----- NOM ET TYPE DU MAITRE
!
        call cfnoap(noma, sdcont_defi, typapp, entapp, nomapp,&
                    type2)
!
! ----- JEU
!
        if (phase .eq. 'INI') then
            jeu = zr(jjeux+3*(iliai-1)+1-1)
        else if (phase.eq.'FIN') then
            jeu = zr(jjeuit+3*(iliai-1)+1-1)
        else
            ASSERT(.false.)
        endif
!
! --- ACTIF OU NON ?
!
        actif = 0
        do iliac = 1, nbliac
            if (zi(jliac-1+iliac) .eq. iliai) then
                actif = 1
            endif
        end do
!
! --- IMPRESSION
!
        if (actif .eq. 1) then
            chaiac = ' ACTIVE (JEU: '
            write (ifm,300) iliai,'(',nomnoe,type2,nomapp,'): ',&
            chaiac,jeu,',TYPE: CONT.     )'
        else
            chaiac = ' LIBRE  (JEU: '
            write (ifm,310) iliai,'(',nomnoe,type2,nomapp,'): ',&
            chaiac,jeu,')'
!
        endif
    end do
!
10  format (' <CONTACT><LIAI> NOMBRE DE LIAISONS POSSIBLES           :', i8)
20  format (' <CONTACT><LIAI> LISTE DES LIAISONS')
101 format (' <CONTACT><LIAI> NOMBRE DE LIAISONS DE CONTACT INITIALES:',i6)
301 format (' <CONTACT><LIAI> NOMBRE DE LIAISONS DE CONTACT FINALES  :',i6)
300 format (' <CONTACT><LIAI> LIAISON ',i5,a1,a16,a4,a8,a3,a14,1pe12.5,a20)
310 format (' <CONTACT><LIAI> LIAISON ',i5,a1,a16,a4,a8,a3,a14,1pe12.5,a1)
!
    call jedema()
!
end subroutine
