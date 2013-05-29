subroutine nmetl2(motfac, sdieto, icham)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    include 'jeveux.h'
    include 'asterc/getvid.h'
    include 'asterfort/assert.h'
    include 'asterfort/copisd.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/nmetcv.h'
    include 'asterfort/nmetnc.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/vtcopy.h'
    character(len=24) :: sdieto
    character(len=16) :: motfac
    integer :: icham
!
! ----------------------------------------------------------------------
!
! ROUTINE GESTION IN ET OUT
!
! LECTURE D'UN CHAMP - CAS CHAMP PAR CHAMP DANS ETAT_INIT
!
! ----------------------------------------------------------------------
!
!
! IN  MOTFAC : MOT-CLEF FACTEUR POUR LIRE ETAT_INIT
! IN  SDIETO : SD GESTION IN ET OUT
! IN  ICHAM  : INDEX DU CHAMP DANS SDIETO
!
!
!
!
    character(len=24) :: ioinfo, iolcha
    integer :: jioinf, jiolch
    integer :: zioch
    character(len=24) :: champ1, champ2
    integer :: ilecc, iret, ibid
    character(len=24) :: chetin, loccha, lochin
    character(len=24) :: motcei, statut
    character(len=24) :: nomcha, nomch0, nomchs, valk(2)
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATION
!
    champ2 = '&&NMETL2.CHAMP.CONVER'
    ilecc = 0
!
! --- ACCES AUX SDS
!
    ioinfo = sdieto(1:19)//'.INFO'
    iolcha = sdieto(1:19)//'.LCHA'
    call jeveuo(ioinfo, 'L', jioinf)
    call jeveuo(iolcha, 'E', jiolch)
    zioch = zi(jioinf+4-1)
!
! --- CHAMP A LIRE ?
!
    chetin = zk24(jiolch+zioch*(icham-1)+8-1)
    if (chetin .eq. 'NON') goto 999
!
! --- NOM DU CHAMP DANS SD RESULTAT
!
    nomchs = zk24(jiolch+zioch*(icham-1)+1-1)
!
! --- NOM DU CHAMP NUL
!
    nomch0 = zk24(jiolch+zioch*(icham-1)+2-1)
!
! --- NOM DU CHAMP DANS OPERATEUR
!
    call nmetnc(sdieto, icham, nomcha)
!
! --- STATUT DU CHAMP
!
    statut = zk24(jiolch+zioch*(icham-1)+4-1)
!
! --- LOCALISATION DU CHAMP
!
    loccha = zk24(jiolch+zioch*(icham-1)+5-1)
!
! --- MOT-CLEF POUR RECUPERER LE CHAMP DANS ETAT_INIT
!
    motcei = zk24(jiolch+zioch*(icham-1)+3-1)
    if (motcei .ne. ' ') then
        call getvid(motfac, motcei, 1, iarg, 1,&
                    champ1, ilecc)
    endif
!
! --- TRAITEMENT DU CHAMP
!
    if (ilecc .eq. 0) then
        if ((statut(1:6).ne.'SDRESU') .and. (nomch0.ne.' ')) then
            call copisd('CHAMP_GD', 'V', nomch0, nomcha)
            zk24(jiolch+zioch*(icham-1)+4-1) = 'ZERO'
        endif
    else
!
! ----- TYPE DU CHAMP DONNE
!
        call dismoi('C', 'TYPE_CHAMP', champ1, 'CHAMP', ibid,&
                    lochin, iret)
!
! ----- CONVERSION EVENTUELLE DU CHAMP
!
        call nmetcv(nomchs, nomch0, lochin, loccha, champ1,&
                    champ2)
!
! ----- RECOPIE DU CHAMP EN LOCAL
!
        if (loccha .eq. 'NOEU') then
            call vtcopy(champ2, nomcha, ' ', iret)
            if (iret .ne. 0) then
                valk(1) = champ1
                valk(2) = nomcha
                call u2mesk('A', 'MECANONLINE_2', 2, valk)
            endif
        else if ((loccha.eq.'ELGA').or.(loccha.eq.'ELEM')) then
            call copisd('CHAMP_GD', 'V', champ2, nomcha)
        else
            write(6,*) 'LOCCHA: ',loccha
            call assert(.false.)
        endif
!
! ----- STATUT DU CHAMP: LU CHAMP PAR CHAMP
!
        zk24(jiolch+zioch*(icham-1)+4-1) = 'CHAMP'
    endif
!
999  continue
!
    call detrsd('CHAMP', champ2)
!
    call jedema()
end subroutine
