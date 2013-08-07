subroutine nmetcr(modele, compor, fonact, sddyna, sdpost,&
                  defico, resoco, sdieto, carele)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/detrsd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/nmetac.h"
#include "asterfort/nmetc0.h"
#include "asterfort/nmetcc.h"
#include "asterfort/nmetci.h"
#include "asterfort/rscrsd.h"
#include "asterfort/wkvect.h"
    character(len=24) :: sdieto, modele, compor
    integer :: fonact(*)
    character(len=19) :: sddyna, sdpost
    character(len=24) :: defico, resoco, carele
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (GESTION IN ET OUT)
!
! CREATION DE LA SD IN ET OUT
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  COMPOR : CARTE COMPORTEMENT
! IN  SDDYNA : SD DYNAMIQUE
! IN  SDPOST : SD POUR POST-TRAITEMENTS (CRIT_STAB ET MODE_VIBR)
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  DEFICO : SD POUR LA DEFINITION DU CONTACT
! IN  RESOCO : SD DE RESOLUTION DU CONTACT
! IN  SDIETO : SD GESTION IN ET OUT
! IN  CARELE : SD CARA_ELEM
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: zioch, nbmax
    parameter    (zioch = 10,nbmax=20 )
    integer :: nbcham, nbchin, nbchou
    character(len=24) :: ioinfo, iolcha
    integer :: jioinf, jiolch
    integer :: icham, ich
    logical :: chaact(nbmax)
    character(len=8) :: result
    character(len=19) :: resu19
    integer :: ichsy, nbnosy
    character(len=24) :: charch, chnoms, nomsym
    character(len=24) :: chetin, chinit, choper
    logical :: lfind
!
    character(len=24) :: nomchs(nbmax), motcob(nbmax)
    character(len=24) :: nomgd(nbmax), motcei(nbmax), loccha(nbmax)
    logical :: larch(nbmax), letin(nbmax)
! -- NOM DU CHAMP DANS LA SD RESULTAT
    data nomchs  /'DEPL'        ,'SIEF_ELGA'   ,'VARI_ELGA'   ,&
     &              'COMPORTEMENT','VITE'        ,'ACCE'        ,&
     &              'INDC_ELEM'   ,'SECO_ELEM'   ,'COHE_ELEM'   ,&
     &              'VALE_CONT'   ,'MODE_FLAMB'  ,'DEPL_VIBR'   ,&
     &              'DEPL_ABSOLU' ,'VITE_ABSOLU' ,'ACCE_ABSOLU' ,&
     &              'FORC_NODA'   ,'STRX_ELGA'   ,'MODE_STAB'   ,&
     &              'FORC_AMOR'   ,'FORC_LIAI'/
! -- NOM DE LA GRANDEUR
    data nomgd   /'DEPL_R','SIEF_R','VARI_R',&
     &              'COMPOR','DEPL_R','DEPL_R',&
     &              'NEUT_I','NEUT_R','NEUT_R',&
     &              'DEPL_R','DEPL_R','DEPL_R',&
     &              'DEPL_R','DEPL_R','DEPL_R',&
     &              'DEPL_R','STRX_R','DEPL_R',&
     &              'DEPL_R','DEPL_R'/
! -- MOT-CLEF DANS ETAT_INIT, ' ' SI PAS DE MOT-CLEF
    data motcei  /'DEPL','SIGM','VARI',&
     &              ' '   ,'VITE','ACCE',&
     &              ' '   ,' '   ,' '   ,&
     &              ' '   ,' '   ,' '   ,&
     &              ' '   ,' '   ,' '   ,&
     &              ' '   ,'STRX',' '   ,&
     &              ' '   ,' '/
! -- LOCALISATION DU CHAMP
    data loccha  /'NOEU','ELGA','ELGA',&
     &              'ELGA','NOEU','NOEU',&
     &              'ELEM','ELEM','ELEM',&
     &              'NOEU','NOEU','NOEU',&
     &              'NOEU','NOEU','NOEU',&
     &              'NOEU','ELGA','NOEU',&
     &              'NOEU','NOEU'/
! -- .TRUE. SI CHAMP EST LU DANS ETAT_INIT
    data letin   /.true. ,.true. ,.true. ,&
     &              .false.,.true. ,.true. ,&
     &              .true. ,.true. ,.true. ,&
     &              .false.,.false.,.false.,&
     &              .true. ,.true. ,.true. ,&
     &              .false.,.true. ,.false.,&
     &              .true. ,.true./
! -- .TRUE. SI CHAMP EST ECRIT DANS ARCHIVAGE
    data larch   /.true. ,.true. ,.true. ,&
     &              .true. ,.true. ,.true. ,&
     &              .true. ,.true. ,.true. ,&
     &              .true. ,.true. ,.true. ,&
     &              .true. ,.true. ,.true. ,&
     &              .false.,.true. ,.true. ,&
     &              .true. ,.true./
! -- MOT-CLEF DANS OBSERVATION, ' ' SI PAS DE MOT-CLEF
    data motcob  /'DEPL'        ,'SIEF_ELGA'   ,'VARI_ELGA'   ,&
     &              ' '           ,'VITE'        ,'ACCE'        ,&
     &              ' '           ,' '           ,' '           ,&
     &              'VALE_CONT'   ,' '           ,' '           ,&
     &              'DEPL_ABSOLU' ,'VITE_ABSOLU' ,'ACCE_ABSOLU' ,&
     &              'FORC_NODA'   ,'STRX_ELGA'   ,' '           ,&
     &              ' '           ,' '/
!
! !!!!!!! NE PAS OUBLIER D'AJOUTER LE CHAMP DANS RSCRSD
!
!
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CREATION DE LA SD GESTION'//&
        ' IN ET OUT'
    endif
!
! --- INITIALISATIONS
!
    nbcham = 0
    nbchin = 0
    nbchou = 0
    do 1 icham = 1, nbmax
        chaact(icham) = .false.
 1  end do
    result = '&&NMETCR'
    resu19 = result
!
! --- ACTIVATION DES CHAMPS A TRAITER SUIVANT FONCTIONNALITES ACTIVEES
!
    call nmetac(fonact, sddyna, defico, nbmax, chaact)
!
! --- DECOMPTE DES CHAMPS
!
    do 20 icham = 1, nbmax
        if (chaact(icham)) then
            nbcham = nbcham + 1
            if (letin(icham)) nbchin = nbchin + 1
            if (larch(icham)) nbchou = nbchou + 1
        endif
20  end do
!
! --- CREATION SD CHAMPS
!
    iolcha = sdieto(1:19)//'.LCHA'
    call wkvect(iolcha, 'V V K24', nbcham*zioch, jiolch)
!
! --- AJOUT DES CHAMPS
!
    ich = 0
    do 30 icham = 1, nbmax
        if (chaact(icham)) then
            ich = ich + 1
            call nmetci(sdieto, zioch, ich, nomchs(icham), nomgd ( icham),&
                        motcei(icham), motcob(icham), loccha(icham), letin (icham),&
                        larch (icham))
        endif
30  end do
    ASSERT(ich.eq.nbcham)
!
! --- NOM DES CHAMPS DANS OP0070
!
    call nmetcc(sdieto, compor, sddyna, sdpost, resoco,&
                nbcham, zioch)
!
! --- NOM DES CHAMPS NULS
!
    call nmetc0(modele, sdieto, compor, resoco, nbcham,&
                zioch, carele)
!
! --- CREATION SD INFOS
!
    ioinfo = sdieto(1:19)//'.INFO'
    call wkvect(ioinfo, 'V V I', 4, jioinf)
    zi(jioinf+1-1) = nbcham
    zi(jioinf+2-1) = nbchin
    zi(jioinf+3-1) = nbchou
    zi(jioinf+4-1) = zioch
!
! --- VERIFICATIONS CONFORMITE SD RESULTAT
!
    call rscrsd('V', result, 'EVOL_NOLI', 1)
    call jelira(resu19//'.DESC', 'NOMMAX', nbnosy)
    do 50 icham = 1, nbcham
        charch = zk24(jiolch+zioch*(icham-1)+9-1)
        chnoms = zk24(jiolch+zioch*(icham-1)+1-1)
        chetin = zk24(jiolch+zioch*(icham-1)+8-1)
        chinit = zk24(jiolch+zioch*(icham-1)+2-1)
        choper = zk24(jiolch+zioch*(icham-1)+6-1)
        if (charch .eq. 'OUI') then
            lfind = .false.
            do 55 ichsy = 1, nbnosy
                call jenuno(jexnum(resu19//'.DESC', ichsy), nomsym)
                if (nomsym .eq. chnoms) lfind = .true.
55          continue
! ------- DECLENCHEMENT DU ASSERT -> OUBLI D'IMPACT DANS RSCRSD !
            ASSERT(lfind)
        endif
! ----- DECLENCHEMENT DU ASSERT -> OUBLI D'IMPACT DANS NMETC0 !
        if (chetin .eq. 'OUI') then
            if (chinit .eq. ' ') ASSERT(.false.)
        endif
! ----- DECLENCHEMENT DU ASSERT -> OUBLI D'IMPACT DANS NMETCC !
        if (choper .eq. ' ') ASSERT(.false.)
50  end do
    call detrsd('RESULTAT', result)
!
    call jedema()
end subroutine
