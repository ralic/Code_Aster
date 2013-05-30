subroutine cfsans(defico, npt, jeux, enti, zone)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    implicit      none
    include 'jeveux.h'
    include 'asterc/r8vide.h'
    include 'asterfort/cfdisl.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mminfr.h'
    include 'asterfort/u2mess.h'
    character(len=24) :: defico
    integer :: npt
    character(len=24) :: jeux, enti, zone
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - VERIF)
!
! AFFICHAGE DES INTERPENETRATIONS
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  NPT    : NOMBRE DE POINTS EN MODE VERIF
! IN  JEUX   : NOM DE LA SD STOCKANT LE JEU
! IN  ENTI   : NOM DE LA SD STOCKANT LES NOMS DES ENTITES APPARIEES
! IN  ZONE   : NOM DE LA SD STOCKANT LA ZONE A LAQUELLE APPARTIENT LE
!              POINT
!
!
!
!
    character(len=16) :: noment, nompt
    integer :: interp
    integer :: ifm, niv
    logical :: lstop
    real(kind=8) :: jeu, jeuref
    integer :: ipt, izone
    integer :: jjeux, jenti, jzone
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- ACCES SD PROVISOIRES
!
    call jeveuo(jeux, 'L', jjeux)
    call jeveuo(zone, 'L', jzone)
    call jeveuo(enti, 'L', jenti)
!
! --- INITIALISATIONS
!
    interp = 0
!
! --- ALARME OU ERREUR ?
!
    lstop = cfdisl(defico,'STOP_INTERP')
!
    do 120 ipt = 1, npt
!
! ----- INFORMATIONS SUR LE POINT
!
        jeu = zr(jjeux+ipt-1)
        izone = zi(jzone+ipt-1)
        nompt = zk16(jenti+2*(ipt-1)+1-1)
        noment = zk16(jenti+2*(ipt-1)+2-1)
!
! ----- OPTIONS VERIF
!
        jeuref = mminfr(defico,'TOLE_INTERP',izone)
!
! ----- TEST DU JEU
!
        if ((jeu.lt.jeuref) .and. (jeu.ne.r8vide())) then
            if (niv .ge. 2) then
                write (ifm,2001) nompt,noment,jeu
            endif
            interp = interp+1
        endif
!
120  end do
!
! --- ALARME OU ERREUR FATALE ?
!
    if (interp .ge. 1) then
        if (niv .ge. 2) then
            write (ifm,3000) interp,jeuref
        endif
        if (lstop) then
            call u2mess('F', 'CONTACT_93')
        else
            call u2mess('A', 'CONTACT_93')
        endif
    endif
!
    2001 format (' <CONTACT>   * INTERPENETRATION DE <',a16,'> AVEC <',&
     &        a16,'> * JEU:',1pe12.5)
    3000 format (' <CONTACT>   * IL Y A ',i6,&
     &        ' NOEUDS INTERPENETRES (JEU REF.: ',1pe12.5,')')
!
! ----------------------------------------------------------------------
!
    call jedema()
!
end subroutine
