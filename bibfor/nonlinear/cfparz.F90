subroutine cfparz(resoco, iliai, coefff, coefpn, coefpt,&
                  coefte, dissup, izone, ip, numnoe,&
                  posnoe)
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
    include 'jeveux.h'
    include 'asterfort/cfmmvd.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=24) :: resoco
    real(kind=8) :: coefff, coefpn, coefpt, coefte, dissup
    integer :: iliai, ip, izone, numnoe, posnoe
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT)
!
! CARACTERISTIQUES DES LIAISONS POUR LA ZONE
!
! ----------------------------------------------------------------------
!
!
! IN  ILIAI  : INDICE DE LA LIAISON COURANTE
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  DISSUP : JEU FICTIF DE LA ZONE
! IN  COEFPN : COEFFICIENT DE PENALISATION DE CONTACT
! IN  COEFPT : COEFFICIENT DE PENALISATION DE FROTTEMENT
! IN  COEFFF : COEFFICIENT DE FROTTEMENT
! IN  COEFTE : COEFFICIENT THETA POUR FROTTEMENT
! IN  POSNOE : INDICES DANS CONTNO DU NOEUD ESCLAVE
! IN  NUMNOE : NUMERO ABSOLU DU NOEUD ESCLAVE
! IN  IP     : INDICE DU POINT DANS LA SD APPARIEMENT
! IN  IZONE  : NUMERO DE LA ZONE DE CONTACT
!
!
!
!
    integer :: ifm, niv
    integer :: ztacf
    character(len=24) :: tacfin, jeusup
    integer :: jtacf, jjsup
    character(len=24) :: jeuite, numlia
    integer :: jjeuit, jnumli
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    jeuite = resoco(1:14)//'.JEUITE'
    tacfin = resoco(1:14)//'.TACFIN'
    jeusup = resoco(1:14)//'.JSUPCO'
    numlia = resoco(1:14)//'.NUMLIA'
!
    call jeveuo(jeuite, 'E', jjeuit)
    call jeveuo(tacfin, 'E', jtacf)
    call jeveuo(jeusup, 'E', jjsup)
    call jeveuo(numlia, 'E', jnumli)
!
    ztacf = cfmmvd('ZTACF')
!
! --- JEU FICTIF DE LA ZONE
!
    zr(jjsup+iliai-1) = dissup
!
! --- ADDITION DU JEU FICTIF DE LA ZONE
!
    zr(jjeuit+3*(iliai-1)+1-1) = zr(jjeuit+3*(iliai-1)+1-1) - dissup
!
! --- RECOPIE CARACTERISTIQUES ZONE -> NOEUDS ESCLAVES
!
    zr(jtacf+ztacf*(iliai-1)+0) = coefff
    zr(jtacf+ztacf*(iliai-1)+1) = coefpn
    zr(jtacf+ztacf*(iliai-1)+2) = coefpt
    zr(jtacf+ztacf*(iliai-1)+3) = coefte
!
! --- SAUVEGARDE DANS LA SD APPARIEMENT
!
    zi(jnumli+4*(iliai-1)+1-1) = ip
    zi(jnumli+4*(iliai-1)+2-1) = posnoe
    zi(jnumli+4*(iliai-1)+3-1) = numnoe
    zi(jnumli+4*(iliai-1)+4-1) = izone
!
!
    call jedema()
end subroutine
