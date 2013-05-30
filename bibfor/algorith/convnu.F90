subroutine convnu(numin, numout, nomvec, base, neqout)
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
!***********************************************************************
!    P. RICHARD     DATE 25/01/92
!-----------------------------------------------------------------------
!  BUT:  <  CONVECRSION DE NUMEROTATION >
!    CREER UN VECTEUR PERMETTANT DE PASSER D'UNE NUMEROTATION
!  A UNE AUTRE, CE VECTEUR DONNE POUR CHAQUEEQUATION DE LA NUMEROTATION
!  RESULTAT LE RANG DE L'EQUTION CORRESPONDANTE DANS LA NUMEROTATION
!   DE DEPART
    implicit none
!
!   SEULS LES DDL PHYSIQUES SONT RESTITUE, DONC SLES LAGRANGES SONT
!   AUTOMATIQUEMENT MIS A ZERO
!
!  CETTE ROUTINE ENVOIE UN MESSAGE D'ALARME SI IL APPARAIT UNE PERTE
!  AU NIVEAU DES DDL PHYSIQUE
!  CE QUI REVIENT A DIRE QUE LA DIFFERENCE DOIT RESIDER
!   DANS LES LAGRANGES
!-----------------------------------------------------------------------
!
! NUMIN    /I/: NOM UT DE LA NUMEROTATION DE DEPART
! NUMOUT   /I/: NOM UT DE LA NUMEROTATION FINALE
! NOMVEC   /I/: NOM K24 DU VECTEUR D'ENTIER RESULTAT
! BASE     /I/: TYPE DE LA BASE JEVEUX 'G' OU 'V'
! NEQOUT   /O/: NOMBRE D'EQUATION DE LA NUMEROTATION RESULTAT
!
!
!
!
    include 'jeveux.h'
    include 'asterfort/cheddl.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/wkvect.h'
    character(len=1) :: base
    character(len=8) :: maiin, maiout
    character(len=8) :: k8bid
    character(len=19) :: numin, numout
    character(len=24) :: nomvec
    character(len=24) :: valk(4)
    logical :: erreur
!
    integer :: ibid
    integer :: vali(2)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iran, iret, ityp, ldcvn, lldein, lldeou
    integer :: nbid, neqin, neqout, nuno
!-----------------------------------------------------------------------
    data ibid/0/
!-----------------------------------------------------------------------
!
!
!
!--------RECUPERATION DES MAILLAGE ET VERIFICATION COMPATIBILITE--------
!
    call jemarq()
    erreur = .false.
    call dismoi('F', 'NOM_MAILLA', numin, 'NUME_DDL', nbid,&
                maiin, iret)
    call dismoi('F', 'NOM_MAILLA', numout, 'NUME_DDL', nbid,&
                maiout, iret)
!
    if (maiin .ne. maiout) then
        valk (1) = numin
        valk (2) = maiin
        valk (3) = numout
        valk (4) = maiout
        call u2mesg('F', 'ALGORITH12_62', 4, valk, 0,&
                    0, 0, 0.d0)
    endif
!
!
!------------RECUPERATION DES DIMENSIONS DES NUMEROTATIONS--------------
!
    call dismoi('F', 'NB_EQUA', numin, 'NUME_DDL', neqin,&
                k8bid, ibid)
    call dismoi('F', 'NB_EQUA', numout, 'NUME_DDL', neqout,&
                k8bid, ibid)
!
!-------------------ALLOCATION DU VECTEUR RESULTAT----------------------
!
    call wkvect(nomvec, base//' V I', neqout, ldcvn)
!
!
!-----------REQUETTE DES DEEQ DES NUMEROTATIONS-------------------------
!
    call jeveuo(numin//'.DEEQ', 'L', lldein)
    call jeveuo(numout//'.DEEQ', 'L', lldeou)
!
!
!------------------BOUCLE SUR LES DDL-----------------------------------
!
    do 10 i = 1, neqout
        nuno=zi(lldeou+2*(i-1))
        ityp=zi(lldeou+2*(i-1)+1)
        if (ityp .gt. 0) then
            call cheddl(zi(lldein), neqin, nuno, ityp, iran,&
                        1)
            if (iran .eq. 0) then
                erreur=.true.
                vali (1) = nuno
                vali (2) = ityp
                call u2mesg('A', 'ALGORITH12_63', 0, ' ', 2,&
                            vali, 0, 0.d0)
            else
                zi(ldcvn+i-1)=iran
            endif
        endif
!
10  end do
!
!--------------------------TRAITEMENT ERREUR EVENTUELLE----------------
!
    if (erreur) then
        call u2mesg('F', 'ALGORITH12_64', 0, ' ', 0,&
                    0, 0, 0.d0)
    endif
!
!------------------------LIBERATION DES OBJETS -------------------------
!
!
    call jedema()
end subroutine
