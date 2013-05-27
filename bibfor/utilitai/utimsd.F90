subroutine utimsd(unit, niveau, lattr, lcont, sch1,&
                  ipos, base)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!     --
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
    include 'asterfort/dbgobj.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jelstc.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/utimob.h'
    include 'asterfort/uttr24.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: sch1, base
    integer :: ipos, niveau, unit
    logical :: lattr, lcont
! ----------------------------------------------------------------------
! BUT:
!   IMPRIMER LE CONTENU DES OBJETS JEVEUX (K24) AYANT
!   LA CHAINE SCH1 EN POSITION IPOS DANS LEURS NOMS.
!
!
! IN:
!  UNIT     : UNITE LOGIQUE D'IMPRESSION
!  NIVEAU   : NIVEAU D'IMPRESSION
!    NIVEAU 0 --> IMPRESSION DES NOMS DES OBJETS.
!    NIVEAU 1 --> IMPRESSION DU CONTENU DES 10 1ER OBJETS DE COLLEC.
!    NIVEAU 2 --> IMPRESSION DU CONTENU DE TOUS LES OBJETS DE COLLEC.
!    NIVEAU -1--> IMPRESSION DU RESUME DES OBJETS (1 LIGNE PAR OBJET)
!  LATTR   : VRAI : ON IMPRIME LES ATTRIBUTS
!          : FAUX : ON N'IMPRIME PAS LES ATTRIBUTS
!  LCONT   : VRAI : ON IMPRIME LE CONTENU DES OBJETS
!          : FAUX : ON N'IMPRIME PAS LE CONTENU DES OBJETS
!   SCH1   : CHAINE DE CARACTERES CHERCHEE
!   IPOS   : / DEBUT DE LA CHAINE DE CARACTERES A CHERCHER (1,...,24)
!            / 0 : ON NE REGARDE PAS SCH1, ON IMPRIME TOUS LES OBJETS
!   BASE   : 'G','V','L',OU ' '(TOUTES)
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=24) :: ob1, chain2
    character(len=40) :: lb
    character(len=1) :: xous, bas2
    integer :: long, nbval, nbobj, ialiob, i, ibid
    logical :: tout
    character(len=8) :: kbid
!
!
    call jemarq()
    bas2= base
!
!     --QUELQUES VERIFICATIONS:
!     -------------------------
    if (ipos .eq. 0) then
        tout=.true.
    else
        tout=.false.
!
        long=len(sch1)
        if (len(sch1) .gt. 24) then
            call u2mess('F', 'UTILITAI5_42')
        endif
        if ((ipos.lt.0) .or. (ipos.gt.24)) then
            call u2mess('F', 'UTILITAI5_43')
        endif
        if (ipos+len(sch1) .gt. 25) then
            call u2mess('F', 'UTILITAI5_44')
        endif
    endif
!
!     -- ECRITURE DE L'ENTETE :
!    --------------------------
    chain2='????????????????????????'
    if (.not.tout) chain2(ipos:ipos-1+long)=sch1
    write(unit,*) ' '
    write(unit,*) '====> IMPR_CO DE LA STRUCTURE DE DONNEE : ',&
     &                chain2
    write(unit,*) 'ATTRIBUT : ',lattr&
     &                   ,' CONTENU : ',lcont,' BASE : >',bas2,'<'
    call jelstc(bas2, sch1, ipos, 0, kbid,&
                nbval)
    nbobj= -nbval
    write(unit,*) 'NOMBRE D''OBJETS (OU COLLECTIONS) TROUVES :',nbobj
    write(unit,*) ' '
    if (nbval .eq. 0) goto 9999
!
!     -- RECHERCHE DES NOMS DES OBJETS VERIFIANT LE CRITERE:
!    -------------------------------------------------------
    call wkvect('&&UTIMSD.LISTE', 'V V K24', nbobj, ialiob)
    call jelstc(bas2, sch1, ipos, nbobj, zk24(ialiob),&
                nbval)
!
!     -- ON TRIE PAR ORDRE ALPHABETIQUE:
    call uttr24(zk24(ialiob), nbobj)
!
!
!     -- SI NIVEAU = 0 ON IMPRIME LES NOMS DES OBJETS :
!     -----------------------------------------------
    if (niveau .eq. 0) then
        do 1 i = 1, nbobj
            ob1 = zk24(ialiob-1+i)
            write(unit,*) '      >',ob1,'<'
 1      continue
!
!
    else if (niveau.eq.-1) then
        do 4 i = 1, nbobj
            ob1 = zk24(ialiob-1+i)
            call dbgobj(ob1, 'OUI', unit, '&&UTIMSD')
 4      continue
!
!
    else if (niveau.gt.0) then
        lb='========================================'
!
!       -- IMPRESSION DES ATTRIBUTS :
!       -----------------------------
        if (lattr) then
            write(unit,'(A40,A40)') lb,lb
            write(unit,*) ' IMPRESSION DES ATTRIBUTS DES OBJETS TROUVES :'
            do 2 i = 1, nbobj
                ob1 = zk24(ialiob-1+i)
                call jelira(ob1, 'XOUS', ibid, xous)
                call utimob(unit, ob1, niveau, .true., .false.,&
                            xous)
 2          continue
        endif
!
!       -- IMPRESSION DES VALEURS :
!       ---------------------------
        if (lcont) then
            write(unit,'(A40,A40)') lb,lb
            write(unit,*) ' IMPRESSION DU CONTENU DES OBJETS TROUVES :'
            do 3 i = 1, nbobj
                ob1 = zk24(ialiob-1+i)
                call jelira(ob1, 'XOUS', ibid, xous)
                call utimob(unit, ob1, niveau, .false., .true.,&
                            xous)
 3          continue
        endif
!
    endif
!
!
    write(unit,*) '====> FIN IMPR_CO DE DE STRUCTURE DE DONNEE : ',&
     &                chain2
!
!
    call jedetr('&&UTIMSD.LISTE')
9999  continue
    call jedema()
end subroutine
