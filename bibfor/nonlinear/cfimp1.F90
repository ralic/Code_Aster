subroutine cfimp1(phase, noma, defico, resoco, ifm)
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
#include "asterfort/apinfi.h"
#include "asterfort/apnomp.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisd.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfnoap.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=8) :: noma
    character(len=24) :: defico, resoco
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
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
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
    character(len=2) :: typlia
    real(kind=8) :: jeu, jeuold, jeuini
    character(len=10) :: typli
    character(len=24) :: jeuite, jeux
    integer :: jjeuit, jjeux
    character(len=19) :: liac, typl
    integer :: jliac, jtypl
    character(len=24) :: numlia
    integer :: jnumli
    character(len=19) :: sdappa
    integer :: typapp, entapp
    integer :: ndimg, nbliai
    integer :: btotal, nbliac, llf, llf1, llf2
    logical :: llagrf, llagrc
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD CONTACT
!
    liac = resoco(1:14)//'.LIAC'
    typl = resoco(1:14)//'.TYPL'
    numlia = resoco(1:14)//'.NUMLIA'
!
    call jeveuo(liac, 'L', jliac)
    call jeveuo(typl, 'L', jtypl)
    call jeveuo(numlia, 'L', jnumli)
!
    jeuite = resoco(1:14)//'.JEUITE'
    jeux = resoco(1:14)//'.JEUX'
    call jeveuo(jeuite, 'L', jjeuit)
    call jeveuo(jeux, 'L', jjeux)
!
! --- SD APPARIEMENT
!
    sdappa = resoco(1:14)//'.APPA'
!
! --- INFORMATIONS SUR CONTACT
!
    llagrf = cfdisl(defico,'FROT_LAGR')
    llagrc = cfdisl(defico,'CONT_LAGR')
    ndimg = cfdisd(resoco,'NDIM' )
    nbliai = cfdisd(resoco,'NBLIAI')
    nbliac = cfdisd(resoco,'NBLIAC')
    llf = cfdisd(resoco,'LLF' )
    llf1 = cfdisd(resoco,'LLF1' )
    llf2 = cfdisd(resoco,'LLF2' )
    btotal = nbliac+llf+llf1+llf2
!
! --- AFFICHAGE EN-TETE
!
    write(ifm,10)  nbliai
    if (phase .eq. 'INI') then
        write(ifm,101) nbliac
    else if (phase.eq.'FIN') then
        write(ifm,301) nbliac
    else
        call assert(.false.)
    endif
!
    if (llagrf) then
        if (ndimg .eq. 2) then
            write(ifm,102) llf
        else if (ndimg.eq.3) then
            write(ifm,202) llf
            write(ifm,203) llf1
            write(ifm,204) llf2
        endif
    endif
!
    write(ifm,20)
!
! --- BOUCLE SUR LES LIAISONS
!
    do 500 iliai = 1, nbliai
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
        call cfnoap(noma, defico, typapp, entapp, nomapp,&
                    type2)
!
! ----- JEU
!
        if (phase .eq. 'INI') then
            jeuini = zr(jjeux+3*(iliai-1)+1-1)
            jeuold = zr(jjeuit+3*(iliai-1)+1-1)
            if (llagrc) then
                jeu = jeuold
            else
                jeu = jeuini
            endif
        else if (phase.eq.'FIN') then
            jeu = zr(jjeuit+3*(iliai-1)+1-1)
        else
            call assert(.false.)
        endif
!
! --- ACTIF OU NON ?
!
        actif = 0
!
        do 90 iliac = 1, btotal
            if (zi(jliac-1+iliac) .eq. iliai) then
                actif = 1
!
! --- TYPE LIAISON
!
                typli = 'CONT.     '
                typlia = zk8(jtypl-1+iliac)(1:2)
                if (typlia .eq. 'C0') then
                    typli = 'CONT.     '
                else if (typlia.eq.'F0') then
                    if (ndimg .eq. 3) then
                        typli = 'ADHE. 1&2 '
                    else
                        typli = 'ADHE.     '
                    endif
                else if (typlia.eq.'F1') then
                    typli = 'ADHE. 1   '
                else if (typlia.eq.'F2') then
                    typli = 'ADHE. 2   '
                endif
            endif
90      continue
!
! --- IMPRESSION
!
        if (actif .eq. 1) then
            chaiac = ' ACTIVE (JEU: '
            write (ifm,1000) iliai,'(',nomnoe,type2,nomapp,'): ',&
            chaiac,jeu,',TYPE: ',typli,')'
        else
            chaiac = ' LIBRE  (JEU: '
            write (ifm,1010) iliai,'(',nomnoe,type2,nomapp,'): ',&
            chaiac,jeu,')'
!
        endif
500  end do
!
    10 format (' <CONTACT><LIAI> NOMBRE DE LIAISONS ',&
     &        'POSSIBLES           :',&
     &       i8)
!
    20 format (' <CONTACT><LIAI> LISTE DES LIAISONS')
!
    101 format (' <CONTACT><LIAI> NOMBRE DE LIAISONS ',&
     &        'DE CONTACT INITIALES:',i6)
!
    102 format (' <CONTACT><LIAI>   DONT ADHERENTES :',&
     &       i8)
!
    202 format (' <CONTACT><LIAI>   DONT ADHERENTES DIR. 1 & 2 :',&
     &       i8)
    203 format (' <CONTACT><LIAI>   DONT ADHERENTES DIR. 1     :',&
     &       i8)
    204 format (' <CONTACT><LIAI>   DONT ADHERENTES DIR. 2     :',&
     &       i8)
!
    301 format (' <CONTACT><LIAI> NOMBRE DE LIAISONS ',&
     &        ' DE CONTACT FINALES  :',i6)
!
!
    1000 format (' <CONTACT><LIAI> LIAISON ',i5,a1,a16,a4,a8,a3,a14,&
     &         1pe12.5,a7,a10,a1)
!
    1010 format (' <CONTACT><LIAI> LIAISON ',i5,a1,a16,a4,a8,a3,a14,&
     &         1pe12.5,a1)
!
    call jedema()
!
end subroutine
