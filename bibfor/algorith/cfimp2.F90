subroutine cfimp2(defico, resoco, noma, iliai, typlia,&
                  typeou)
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
    implicit     none
    include 'jeveux.h'
    include 'asterfort/apinfi.h'
    include 'asterfort/apnomp.h'
    include 'asterfort/assert.h'
    include 'asterfort/cfdisd.h'
    include 'asterfort/cfdisr.h'
    include 'asterfort/cfnoap.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=8) :: noma
    character(len=24) :: defico, resoco
    integer :: iliai
    character(len=2) :: typlia
    character(len=3) :: typeou
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE - APPARIEMENT - UTILITAIRE)
!
! IMPRESSION DE L'ACTIVATION/DESACTIVATION DE LA LIAISON ESCLAVE/MAITRE
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NOMA   : NOM DU MAILLAGE
! IN  ILIAI  : NUMERO DE LA LIAISON (INDICE DANS LE TABLEAU GLOBAL DE
!              TOUTE LES LIAISONS POSSIBLES -APPARIEES-)
! IN  TYPLIA : TYPE DE LA LIAISON
!                'C0': CONTACT
!                'F0': FROTTEMENT SUIVANT LES DEUX DIRECTIONS
!                       SIMULTANEES (3D)
!                'F1': FROTTEMENT SUIVANT LA PREMIERE DIRECTION (3D)
!                'F2': FROTTEMENT SUIVANT LA SECONDE DIRECTION (3D)
!                'F3': FROTTEMENT (2D)
! IN  TYPEOU : LIEU OU L'OPERATION A ETE FAITE
!                'ACT' : ACTIVATION LIAISON DE CONTACT
!                'LIB' : DESACTIVATION LIAISON DE CONTACT
!                'NEG' : SUPPRESSION D'UNE LIAISON A PRESSION NEGATIVE
!                'GLI' : SUPPRESSION D'UNE LIAISON GLISSANTE
!                'ADH' : AJOUT D'UNE LIAISON ADHERENTE
!                'PIV' : SUPPRESSION D'UNE LIAISON A PIVOT NUL
!                'ALJ' : ALARME LORSQU'UN JEU DEPASSE LA VALEUR SEUIL
!                         DANS LE CAS DU CONTACT GLISSIERE
!                'SIN' : AFFICHAGE DE LA LIAISON PROVOQUANT UNE MATRICE
!                         DE CONTACT SINGULIERE
!                'AGC' : ALARME LORSQUE LE NOMBRE D'ITERATIONS MAX
!                         DU GCP EST DEPASSE
!
!
!
!
    integer :: ifm, niv
    character(len=24) :: numlia
    integer :: jnumli
    integer :: ip, izone
    integer :: entapp
    character(len=8) :: nomapp
    character(len=16) :: nomnoe
    integer :: typapp
    character(len=16) :: etalia
    character(len=4) :: type2
    character(len=19) :: sdappa
    character(len=38) :: nomlia
    real(kind=8) :: aljeu
    character(len=10) :: typli
    integer :: ndim
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- ACCES SD CONTACT
!
    numlia = resoco(1:14)//'.NUMLIA'
    call jeveuo(numlia, 'L', jnumli)
!
! --- INITIALISATIONS
!
    aljeu = cfdisr(defico,'ALARME_JEU' )
    nomlia = ' '
    etalia = ' '
    ndim = cfdisd(resoco,'NDIM' )
!
! --- SD APPARIEMENT
!
    sdappa = resoco(1:14)//'.APPA'
!
    if (niv .ge. 2) then
!
! ----- POINT DE CONTACT
!
        ip = zi(jnumli+4*(iliai-1)+1-1)
!
! ----- NOM DU NOEUD ESCLAVE
!
        call apnomp(sdappa, ip, nomnoe)
!
! ----- INFOS APPARIEMENT
!
        call apinfi(sdappa, 'APPARI_TYPE', ip, typapp)
        call apinfi(sdappa, 'APPARI_ENTITE', ip, entapp)
        call apinfi(sdappa, 'APPARI_ZONE', ip, izone)
!
! ----- NOM ET TYPE DU MAITRE
!
        call cfnoap(noma, defico, typapp, entapp, nomapp,&
                    type2)
!
! ----- NOM DE LA LIAISON
!
        write (nomlia,100) iliai,'( ',nomnoe,type2,nomapp,'): '
!
! ----- TYPE LIAISON
!
        typli = 'CONT.     '
        if (typlia .eq. 'C0') then
            typli = 'CONT.     '
        else if (typlia.eq.'F0') then
            if (ndim .eq. 3) then
                typli = 'ADHE. 1&2 '
            else
                typli = 'ADHE.     '
            endif
        else if (typlia.eq.'F1') then
            typli = 'ADHE. 1   '
        else if (typlia.eq.'F2') then
            typli = 'ADHE. 2   '
        endif
!
! ----- AFFICHAGE LIAISON
!
        if (typeou .eq. 'ACT') then
            etalia = ' CONT - AJOUTE  '
            write (ifm,1002) nomlia,etalia
!
        else if (typeou.eq.'LIB') then
            etalia = ' CONT - SUPPRIME'
            write (ifm,1002) nomlia,etalia
!
        else if (typeou.eq.'NEG') then
            etalia = ' PRES. NEGATIVE '
            write (ifm,1000) nomlia,etalia,' - TYPE ',typli
!
        else if (typeou.eq.'PIV') then
            etalia = ' PIVOT NUL      '
            write (ifm,1000) nomlia,etalia,' - TYPE ',typli
!
        else if (typeou.eq.'GLI') then
            etalia = ' GLIS - SUPPRIME'
            write (ifm,1000) nomlia,etalia,' - TYPE ',typli
!
        else if (typeou.eq.'ADH') then
            etalia = ' ADHE - AJOUTE  '
            write (ifm,1000) nomlia,etalia,' - TYPE ',typli
!
        else if (typeou.eq.'ALJ') then
            etalia = ' DECOLLE DU JEU '
            write (ifm,1001) nomlia,etalia,aljeu
!
        else if (typeou.eq.'AGC') then
            etalia = ' INTERPENETRE   '
            write (ifm,1002) nomlia,etalia
!
        else
            call assert(.false.)
        endif
    endif
!
    100 format (i5,a2,a16,a4,a8,a3)
    1000 format (' <CONTACT><CALC> LIAISON ',a38,a16,a8,a10)
    1001 format (' <CONTACT><CALC> LIAISON ',a38,a16,e12.3)
    1002 format (' <CONTACT><CALC> LIAISON ',a38,a16)
!
    call jedema()
!
end subroutine
