subroutine ntetcr(numedd, compor, sdieto, lnonl, vhydr,&
                  hydr0)
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
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmetci.h"
#include "asterfort/nthydr.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
    character(len=24) :: numedd, sdieto, compor
    character(len=24) :: vhydr
    logical(kind=1) :: lnonl
    character(len=24) :: hydr0
!
! ----------------------------------------------------------------------
!
! ROUTINE THER_* (GESTION IN ET OUT)
!
! CREATION DE LA SD IN ET OUT
!
! ----------------------------------------------------------------------
!
!
! IN  COMPOR : CARTE COMPORTEMENT
! IN  SDIETO : SD GESTION IN ET OUT
!
!
!
!
    integer :: zioch, nbmax
    parameter    (zioch = 10,nbmax=3 )
    integer :: nbcham, nbchin, nbchou
    character(len=24) :: ioinfo, iolcha
    integer :: jioinf, jiolch
    integer :: icham, ich, neq
    logical(kind=1) :: lhydr
    character(len=19) :: temp0
    character(len=24) :: nomcha, nomchx, nomch0
    logical(kind=1) :: chaact(nbmax)
!
    character(len=24) :: nomchs(nbmax), motcob(nbmax)
    character(len=24) :: nomgd(nbmax), motcei(nbmax), loccha(nbmax)
    logical(kind=1) :: larch(nbmax), letin(nbmax)
! -- NOM DU CHAMP DANS LA SD RESULTAT
    data nomchs  /'TEMP'        ,'HYDR_ELNO'   ,'COMPORTHER'  /
! -- NOM DE LA GRANDEUR
    data nomgd   /'TEMP_R','HYDR_R','COMPOR'/
! -- MOT-CLEF DANS ETAT_INIT, ' ' SI PAS DE MOT-CLEF
    data motcei  /'CHAM_NO',' ',' '/
! -- LOCALISATION DU CHAMP
    data loccha  /'NOEU','ELNO','ELGA'/
! -- .TRUE. SI CHAMP EST LU DANS ETAT_INIT
    data letin   /.true. ,.true. ,.false./
! -- .TRUE. SI CHAMP EST ECRIT DANS ARCHIVAGE
    data larch   /.true. ,.true. ,.true. /
! -- MOT-CLEF DANS OBSERVATION, ' ' SI PAS DE MOT-CLEF
    data motcob  /'TEMP'        ,' '   ,' '/
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nbcham = 0
    nbchin = 0
    nbchou = 0
    do 1 icham = 1, nbmax
        chaact(icham) = .false.
 1  end do
    temp0 = '&&NTETCR.TEMP0'
!
! --- FONCTIONNALITES ACTIVEES
!
    lhydr = .false.
    if (lnonl) call nthydr(lhydr)
!
! --- PREPARATION CHAMP NUL
!
    call vtcreb(temp0, numedd, 'V', 'R', neq)
!
! --- CHAMPS STANDARD
!
    chaact(1) = .true.
!
! --- CARTE COMPORTEMENT
!
    if (lnonl) then
        chaact(3) = .true.
    endif
!
! --- CHAMP HYDRATATION
!
    if (lhydr) then
        chaact(2) = .true.
    endif
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
! --- NOM DU CHAMP
!
    do 40 icham = 1, nbcham
        nomcha = zk24(jiolch+zioch*(icham-1)+1-1)
        if (nomcha .eq. 'TEMP') then
            nomchx = 'CHAP#VALINC#TEMP'
        else if (nomcha.eq.'HYDR_ELNO') then
            nomchx = vhydr
        else if (nomcha.eq.'COMPORTHER') then
            nomchx = compor
        else
            write(6,*) 'NOMCHA: ',nomcha
            ASSERT(.false.)
        endif
        zk24(jiolch+zioch*(icham-1)+6-1) = nomchx
40  end do
!
! --- NOM DU CHAMP NUL
!
    do 50 icham = 1, nbcham
        nomcha = zk24(jiolch+zioch*(icham-1)+1-1)
        if (nomcha .eq. 'TEMP') then
            nomch0 = temp0
        else if (nomcha.eq.'HYDR_ELNO') then
            nomch0 = hydr0
        else if (nomcha.eq.'COMPORTHER') then
            nomch0 = ' '
        else
            write(6,*) 'NOMCHA: ',nomcha
            ASSERT(.false.)
        endif
        zk24(jiolch+zioch*(icham-1)+2-1) = nomch0
50  end do
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
    call jedema()
end subroutine
