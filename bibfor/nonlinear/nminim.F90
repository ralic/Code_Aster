subroutine nminim(sdsuiv, sdimpr)
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/impfoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/obclcr.h"
#include "asterfort/obcrea.h"
#include "asterfort/obgetb.h"
#include "asterfort/obgeti.h"
#include "asterfort/oblcre.h"
#include "asterfort/oblgen.h"
#include "asterfort/oblgoi.h"
#include "asterfort/oblsoi.h"
#include "asterfort/obsetb.h"
#include "asterfort/obseti.h"
#include "asterfort/obseto.h"
#include "asterfort/obtran.h"
#include "asterfort/ulopen.h"
#include "asterfort/utmess.h"
    character(len=24) :: sdimpr, sdsuiv
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - INITIALISATIONS)
!
! INITIALISATION DES INFORMATIONS POUR L'IMPRESSION
!
! ----------------------------------------------------------------------
!
!
! IN  SDIMPR : SD AFFICHAGE
! IN  SDSUIV : SD SUIVI_DDL
!
! ----------------------------------------------------------------------
!
    integer :: nbcolo, larcol, tithau
    parameter    (nbcolo=29,larcol=16,tithau=3)
    character(len=15) :: typcoz(nbcolo), ordcoz(nbcolo)
    character(len=16) :: titr1z(nbcolo), titr2z(nbcolo), titr3z(nbcolo)
    character(len=1) :: typvaz(nbcolo)
    character(len=4) :: cnovaz(nbcolo)
!
    integer :: ifm, niv
    character(len=24) :: sdtabc, slcolo, sdcolo
    character(len=15) :: typcol, ordcol
    character(len=16) :: titli1, titli2, titli3
    character(len=1) :: typval, indsui
    integer :: icolo, icolo1, icolo2, isuiv, unite, titcom, titco1
    logical :: lcsv, lprint
    character(len=24) :: suiinf
    integer :: jsuiin
    integer :: nbcolt, nbsuiv
    character(len=24) :: ddltit
    integer :: jddlti
    character(len=4) :: cnoval
!
! --- ORDRE DE DEFINITION DONNE L'ORDRE D'AFFICHAGE
!
    data ordcoz /'INCR_INST','BOUC_GEOM','BOUC_FROT',&
     &             'BOUC_CONT','ITER_NUME','RESI_RELA',&
     &             'RELA_NOEU','RESI_MAXI','MAXI_NOEU',&
     &             'RESI_REFE','REFE_NOEU','RESI_COMP',&
     &             'COMP_NOEU','RELI_NBIT','RELI_COEF',&
     &             'PILO_COEF','MATR_ASSE','DEBORST  ',&
     &             'CTCD_NBIT','CONT_NEWT','FROT_NEWT',&
     &             'GEOM_NEWT','CTCC_CYCL','BOUC_VALE',&
     &             'BOUC_NOEU','FROT_NOEU','GEOM_NOEU',&
     &             'FETI_NBIT','ITER_TIME'/
!
    data typcoz /'ITER_NUME','INCR_INST','RESI_RELA',&
     &             'RESI_MAXI','RESI_REFE','RESI_COMP',&
     &             'RELA_NOEU','MAXI_NOEU','REFE_NOEU',&
     &             'COMP_NOEU','RELI_NBIT','RELI_COEF',&
     &             'PILO_COEF','MATR_ASSE','DEBORST  ',&
     &             'CTCD_NBIT','BOUC_GEOM','BOUC_FROT',&
     &             'BOUC_CONT','CONT_NEWT','FROT_NEWT',&
     &             'GEOM_NEWT','CTCC_CYCL','BOUC_VALE',&
     &             'BOUC_NOEU','FROT_NOEU','GEOM_NOEU',&
     &             'FETI_NBIT','ITER_TIME'/
!
    data titr1z /&
     &         '     NEWTON     ','   INCREMENT    ','     RESIDU     ',&
     &         '     RESIDU     ','     RESIDU     ','     RESIDU     ',&
     &         ' RESI_GLOB_RELA ',' RESI_GLOB_MAXI ',' RESI_REFE_RELA ',&
     &         ' RESI_COMP_RELA ','  RECH.  LINE.  ','  RECH.  LINE.  ',&
     &         '    PILOTAGE    ','     OPTION     ','     DEBORST    ',&
     &         '     CONTACT    ','     CONTACT    ','     CONTACT    ',&
     &         '     CONTACT    ','     CONTACT    ','     CONTACT    ',&
     &         '     CONTACT    ','     CONTACT    ','     CONTACT    ',&
     &         '     CONTACT    ','     CONTACT    ','     CONTACT    ',&
     &         '     FETI       ','     NEWTON     '/
!
    data titr2z /&
     &         '    ITERATION   ','    INSTANT     ','     RELATIF    ',&
     &         '     ABSOLU     ','  PAR REFERENCE ',' PAR COMPOSANTE ',&
     &         '     MAXIMUM    ','     MAXIMUM    ','     MAXIMUM    ',&
     &         '     MAXIMUM    ','    NB. ITER    ','  COEFFICIENT   ',&
     &         '  COEFFICIENT   ','   ASSEMBLAGE   ','                ',&
     &         '    DISCRET     ','    BCL. GEOM.  ','    BCL. FROT.  ',&
     &         '    BCL. CONT.  ','   NEWTON GENE  ','   NEWTON GENE  ',&
     &         '   NEWTON GENE  ','      INFOS     ','     CRITERE    ',&
     &         '     CRITERE    ','   NEWTON GENE  ','   NEWTON GENE  ',&
     &         '    NB. ITER    ','  TEMPS CALCUL  '/
!
    data titr3z /&
     &         '                ','                ',' RESI_GLOB_RELA ',&
     &         ' RESI_GLOB_MAXI ',' RESI_REFE_RELA ',' RESI_COMP_RELA ',&
     &         '    AU POINT    ','    AU POINT    ','    AU POINT    ',&
     &         '    AU POINT    ','                ','      RHO       ',&
     &         '      ETA       ','                ','                ',&
     &         '    NB. ITER    ','    ITERATION   ','    ITERATION   ',&
     &         '    ITERATION   ','   VARI. CONT.  ','   CRIT. FROT.  ',&
     &         '   CRIT. GEOM.  ','    CYCLAGES    ','    VALEUR      ',&
     &         '    MAX. LIEU   ',' LIEU MAX FROT. ',' LIEU MAX GEOM. ',&
     &         '                ','                '/
!
    data typvaz /'I','R','R',&
     &             'R','R','R',&
     &             'K','K','K',&
     &             'K','I','R',&
     &             'R','K','K',&
     &             'I','I','I',&
     &             'I','I','R',&
     &             'R','K','R',&
     &             'K','K','K',&
     &             'I','R'/
!
    data cnovaz /'ERRE','ERRE','SANS',&
     &             'SANS','SANS','ERRE',&
     &             'ERRE','ERRE','SANS',&
     &             'SANS','SANS','SANS',&
     &             'VIDE','VIDE','ERRE',&
     &             'ERRE','ERRE','ERRE',&
     &             'ERRE','ERRE','ERRE',&
     &             'ERRE','SANS','SANS',&
     &             'SANS','SANS','ERRE',&
     &             'SANS','ERRE'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... INITIALISATIONS IMPRESSIONS'
    endif
!
! --- TABLEAU DE CONVERGENCE
!
    sdtabc = '&&NMINIM.SDTABC'
    call obcrea('TABLEAU', sdtabc)
    call obseto(sdimpr, 'TABLEAU_CONV', sdtabc)
!
! --- TRANSFERT DES PARAMETRES
!
    call obtran(sdimpr, 'TABL_CONV_CSV', sdtabc, 'SORTIE_CSV')
    call obtran(sdimpr, 'UNIT_CONV_CSV', sdtabc, 'UNITE_CSV')
!
! --- NOM DE LA LISTE DES COLONNES DISPONIBLES
!
    slcolo = '&&NMINIM.SLCOLO'
    call obseto(sdtabc, 'COLONNES_DISPOS', slcolo)
!
! --- NOMBRE DE COLONNES POUR LES SUIVIS EN TEMPS REEL
!
    suiinf = sdsuiv(1:14)//'     .INFO'
    call jeveuo(suiinf, 'L', jsuiin)
    nbsuiv = zi(jsuiin+2-1)
    if (nbsuiv .gt. 9) then
        call utmess('F', 'IMPRESSION_3', si=nbsuiv)
    endif
!
! --- TITRE DES COLONNES POUR LES SUIVIS EN TEMPS REEL
!
    if (nbsuiv .ne. 0) then
        ddltit = sdsuiv(1:14)//'     .TITR'
        call jeveuo(ddltit, 'L', jddlti)
    endif
!
! --- NOMBRE TOTAL DE COLONNES
!
    nbcolt = nbcolo + nbsuiv
!
! --- CREATION DE LA LISTE DES COLONNES DISPONIBLES
!
    call oblcre(slcolo, 'TABLEAU_COLONNE', 'TYPE_COLONNE', nbcolt)
!
! --- CREATION DE TOUTES LES COLONNES DISPONIBLES
!
    do 10 icolo = 1, nbcolo
        typcol = typcoz(icolo)
        titli1 = titr1z(icolo)
        titli2 = titr2z(icolo)
        titli3 = titr3z(icolo)
        typval = typvaz(icolo)
        cnoval = cnovaz(icolo)
!
! ----- GENERATION D'UNE COLONNE
!
        call obclcr('NMINIM', typcol, larcol, tithau, titli1,&
                    titli2, titli3, typval, cnoval, sdcolo)
10  end do
!
! --- AFFECTATION DES COLONNES DANS LA LISTE (DANS L'ORDRE !)
!
    do 15 icolo1 = 1, nbcolo
        ordcol = ordcoz(icolo1)
        icolo = 0
        do 16 icolo2 = 1, nbcolo
            typcol = typcoz(icolo2)
            if (typcol .eq. ordcol) icolo = icolo2
16      continue
        ASSERT(icolo.ne.0)
        typcol = typcoz(icolo)
!
! ----- RECUPERATION D'UNE COLONNE GENEREE
!
        call oblgen('NMINIM', typcol, sdcolo)
!
! ----- AFFECTATION DE LA COLONNE
!
        call oblsoi(slcolo, typcol, sdcolo)
15  end do
!
! --- CREATION DES COLONNES POUR LE SUIVI EN TEMPS REEL
!
    do 20 isuiv = 1, nbsuiv
        call impfoi(0, 1, isuiv, indsui)
        typcol = 'SUIVDDL'//indsui
        titli1 = zk16(jddlti+3*(isuiv-1)+1-1)
        titli2 = zk16(jddlti+3*(isuiv-1)+2-1)
        titli3 = zk16(jddlti+3*(isuiv-1)+3-1)
        typval = 'R'
        cnoval = 'ERRE'
        call obclcr('NMINIM', typcol, larcol, tithau, titli1,&
                    titli2, titli3, typval, cnoval, sdcolo)
20  end do
!
! --- AFFECTATION DES COLONNES DANS LA LISTE
!
    do 25 isuiv = 1, nbsuiv
        call impfoi(0, 1, isuiv, indsui)
        typcol = 'SUIVDDL'//indsui
!
! ----- RECUPERATION D'UNE COLONNE GENEREE
!
        call oblgen('NMINIM', typcol, sdcolo)
!
! ----- AFFECTATION DE LA COLONNE
!
        call oblsoi(slcolo, typcol, sdcolo)
25  end do
!
! --- RECUPERATION HAUTEUR TITRE COMMUNE POUR TABLEAU
!
    call oblgoi(slcolo, 1, sdcolo)
    call obgeti(sdcolo, 'HAUTEUR_TITRE', titco1)
    do 30 icolo = 2, nbcolt
        call oblgoi(slcolo, icolo, sdcolo)
        call obgeti(sdcolo, 'HAUTEUR_TITRE', titcom)
        if (titco1 .ne. titcom) ASSERT(.false.)
30  end do
    call obseti(sdtabc, 'HAUTEUR_TITRE', titcom)
!
! --- OUVERTURE DU FICHIER
!
    call obgetb(sdtabc, 'SORTIE_CSV', lcsv)
    call obgeti(sdtabc, 'UNITE_CSV', unite)
    if (lcsv) call ulopen(unite, ' ', ' ', 'NEW', 'O')
!
! --- AFFICHAGE A CHAQUE PAS ACTIVE PAR DEFAUT
!
    lprint = .true.
    call obsetb(sdimpr, 'PRINT', lprint)
!
    call jedema()
!
end subroutine
