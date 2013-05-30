subroutine cfimp3(defico, resoco, noma, ifm, numord,&
                  instap, nbliai, nbliac, jcnsvr)
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
    include 'jeveux.h'
    include 'asterfort/apinfi.h'
    include 'asterfort/apnomp.h'
    include 'asterfort/cfmmvd.h'
    include 'asterfort/cfnoap.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=24) :: defico, resoco
    integer :: ifm
    integer :: numord
    integer :: nbliai
    integer :: nbliac
    integer :: jcnsvr
    character(len=8) :: noma
    real(kind=8) :: instap
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE - APPARIEMENT - UTILITAIRE)
!
! IMPRESSION DES INFOS SUR LES LIAISONS DE CONTACT/FROTTEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  IFM    : UNITE D'IMPRESSION DU MESSAGE
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NUMORD : NUMERO D'INSTANT
! IN  INSTAP : INSTANT DE CALCUL
! IN  NBLIAI : NOMBRE DE LIAISONS DE CONTACT POSSIBLES
! IN  NBLIAC : NOMBRE DE LIAISONS DE CONTACT ACTIVES
! IN  JCNSVR : POINTEUR SUR OBJET CHAM_NO VALE_CONT
!
!
!
!
    character(len=24) :: numlia
    integer :: jnumli
    integer :: zresu
    integer :: iliai, izone, ip
    integer :: entapp
    character(len=8) :: nomapp
    integer :: numnoe, typapp
    character(len=4) :: type2
    character(len=24) :: jeuite
    integer :: jjeuit
    character(len=16) :: nompt
    real(kind=8) :: rn, r, coe, prod, varc, ajeuft, jeu
    character(len=19) :: sdappa
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD CONTACT
!
    zresu = cfmmvd('ZRESU')
!
! --- ACCES SD CONTACT
!
    numlia = resoco(1:14)//'.NUMLIA'
    jeuite = resoco(1:14)//'.JEUITE'
    call jeveuo(numlia, 'L', jnumli)
    call jeveuo(jeuite, 'L', jjeuit)
!
! --- PREMIERS AFFICHAGES
!
    write(ifm,1000)
    write(ifm,1001) numord
    write(ifm,1002) instap
    write(ifm,1003) nbliai
    write(ifm,1004) nbliac
!
! --- SD APPARIEMENT
!
    sdappa = resoco(1:14)//'.APPA'
!
! --- BOUCLE SUR LES LIAISONS
!
    do 120 iliai = 1, nbliai
!
! ----- POINT DE CONTACT
!
        ip = zi(jnumli+4*(iliai-1)+1-1)
!
! ----- NOEUS ESCLAVE
!
        numnoe = zi(jnumli+4*(iliai-1)+3-1)
!
! ----- NOM DU NOEUD ESCLAVE
!
        call apnomp(sdappa, ip, nompt)
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
! ----- VALEURS
!
        rn = zr(jcnsvr-1+(numnoe-1)*zresu+3 )
        r = zr(jcnsvr-1+(numnoe-1)*zresu+19)
!
        coe = 0.0d0
        if (rn .ne. 0.d0) then
            prod = (abs(r)-abs(rn))
            coe = prod/rn
        endif
!
        varc = zr(jcnsvr-1+ (numnoe-1)*zresu+1 )
        ajeuft = zr(jcnsvr-1+ (numnoe-1)*zresu+9 )
!
! --- JEU
!
        jeu = zr(jjeuit+3*(iliai-1)+1-1)
        if (varc .ne. 0.0d0) then
            write (ifm,2000) iliai,' (   ',nompt ,type2,nomapp,&
     &                   ') * JEU:',jeu,' * RN:',rn,&
     &                   ' * GLI:',ajeuft,' * R:',r,&
     &                   ' * RT/RN:',coe
        else
            write (ifm,2001) iliai,' (   ',nompt ,type2,nomapp,&
     &                   ') * JEU:',jeu,' * RN:',rn,&
     &                   ' * GLI:',ajeuft,' * R:',r,&
     &                   ' * RT/RN:',coe
        endif
120  end do
!
    1000 format (' <CONTACT> LISTE DES LIAISONS DE CONTACT')
    1001 format (' <CONTACT>   NUMERO D''ORDRE  : ',i6)
    1002 format (' <CONTACT>   INSTANT          : ',1pe12.5)
    1003 format (' <CONTACT>   LIAISONS         : ',i6)
    1004 format (' <CONTACT>   LIAISONS ACTIVES : ',i6)
    2000 format (' <CONTACT>   * LIAISON ACTIVE   ',i6,a5,a16,a4,a8,&
     &        a8,1pe12.5,a6,1pe12.5,&
     &        a7,1pe12.5,a5,1pe12.5,&
     &        a9,1pe12.5)
    2001 format (' <CONTACT>   * LIAISON INACTIVE ',i6,a5,a16,a4,a8,&
     &        a8,1pe12.5,a6,1pe12.5,&
     &        a7,1pe12.5,a5,1pe12.5,&
     &        a9,1pe12.5)
!
    call jedema()
end subroutine
