subroutine impsdl(sdtabc, sepcol, uimpr)
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
    include 'asterfort/assert.h'
    include 'asterfort/impfoi.h'
    include 'asterfort/impfok.h'
    include 'asterfort/impfor.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/obgetb.h'
    include 'asterfort/obgeti.h'
    include 'asterfort/obgetk.h'
    include 'asterfort/obgeto.h'
    include 'asterfort/obgetr.h'
    include 'asterfort/oblgai.h'
    include 'asterfort/oblgoi.h'
    include 'asterfort/obtlig.h'
    character(len=24) :: sdtabc
    character(len=1) :: sepcol
    integer :: uimpr
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (GESTION STRUCTS - TABLEAU POUR IMPRESSION)
!
! IMPRESSION D'UNE LIGNE DU TABLEAU
!
! ----------------------------------------------------------------------
!
!
! IN  SDTABC : SD TABLEAU POUR IMPRESSION
! IN  SEPCOL : SEPARATEUR DE COLONNE
! IN  UIMPR  : UNITE D'IMPRESSION
!
! ----------------------------------------------------------------------
!
    integer :: icol, ncol
    integer :: vali, unibid
    integer :: pos, posfin, posmar
    character(len=16) :: chsobj, chvide, valk, typcol
    real(kind=8) :: valr
    character(len=255) :: ligne
    integer :: longr, precr, longi
    character(len=24) :: slcolo, sdcolo
    logical :: lacti, linte, lreel, lchai
    logical :: laffe, lnvvid, lnverr, lnvsan
    integer :: larcol, larlig
    character(len=1) :: marq
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    chsobj = ' - SANS OBJET - '
    chvide = ' '
    unibid = 0
    pos = 2
    longr = 12
    precr = 5
    longi = 6
!
! --- LISTE DES COLONNES DISPONIBLES
!
    call obgeto(sdtabc, 'COLONNES_DISPOS', slcolo)
!
! --- NOMBRE ET LARGEUR DES COLONNES, LARGEUR TOTALE
!
    call obgeti(slcolo, 'NBRE_STRUCTS', ncol)
    call obgeti(sdtabc, 'LARGEUR_LIGNE', larlig)
    call assert(larlig.le.255)
!
! --- INITIALISATION DE LA LIGNE AVEC LES SEPARATEURS DE COLONNE
!
    call obtlig(sdtabc, sepcol, ligne)
!
! --- REMPLISSAGE DE LA LIGNE AVEC LES VALEURS ET LES MARQUES
!
    do 100 icol = 1, ncol
        call oblgoi(slcolo, icol, sdcolo)
        call oblgai(slcolo, icol, lacti)
        if (lacti) then
            call obgeti(sdcolo, 'LARGEUR', larcol)
            call obgetb(sdcolo, 'ENTIER', linte)
            call obgetb(sdcolo, 'REEL', lreel)
            call obgetb(sdcolo, 'CHAINE', lchai)
            call obgetb(sdcolo, 'VALE_AFFE', laffe)
            call obgetk(sdcolo, 'TYPE_COLONNE', typcol)
!
            posfin = larcol+pos-1
!
! ------- SI VALEUR NON AFFECTEEE DANS LA COLONNE
!
            marq = ' '
            if (.not.laffe) then
                call obgetb(sdcolo, 'NON_AFFE_ERREUR', lnverr)
                call obgetb(sdcolo, 'NON_AFFE_VIDE', lnvvid)
                call obgetb(sdcolo, 'NON_AFFE_SANSOBJ', lnvsan)
                if (lnverr) then
                    write(6,*) 'COLONNE: ',typcol
                    write(6,*) 'ERREUR - VALEUR NON AFFECTEE SUR COLONNE'
!              CALL ASSERT(.FALSE.)
                else if (lnvvid) then
                    ligne(pos:posfin) = chvide(1:larcol)
                else if (lnvsan) then
                    ligne(pos:posfin) = chsobj(1:larcol)
                else
                    call assert(.false.)
                endif
            else
                call obgetk(sdcolo, 'MARQUE', marq)
                if (linte) then
                    call obgeti(sdcolo, 'VALE_I', vali)
                    call impfoi(unibid, longi, vali, ligne(pos:posfin))
                else if (lreel) then
                    call obgetr(sdcolo, 'VALE_R', valr)
                    call impfor(unibid, longr, precr, valr, ligne(pos: posfin))
                else if (lchai) then
                    call obgetk(sdcolo, 'VALE_K', valk)
                    ligne(pos:posfin) = valk(1:larcol)
                else
                    call assert(.false.)
                endif
            endif
!
! ------- AJOUT DE LA MARQUE
!
            if (marq(1:1) .ne. ' ') then
                posmar = pos + larcol - 2
                ligne(posmar:posmar) = marq(1:1)
            endif
!
            pos = pos + larcol + 1
        endif
100  end do
!
! --- IMPRESSION DE LA LIGNE DU TABLEAU
!
    call impfok(ligne, larlig, uimpr)
!
    call jedema()
end subroutine
