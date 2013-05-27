subroutine crasse()
    implicit  none
! ----------------------------------------------------------------------
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
!
!     COMMANDE:  CREA_RESU
!     CREE UNE STRUCTURE DE DONNEE DE TYPE "EVOL_THER"
!     PAR ASSEMBLAGES D'EVOL_THER EXISTANTS
!
!
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterfort/assert.h'
    include 'asterfort/copisd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rscrsd.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsmena.h'
    include 'asterfort/rsnoch.h'
    include 'asterfort/u2mesr.h'
!
!
    integer :: iret, nbfac, iocc, nbord1, iord2, iord1
    integer :: kord1, iad, jord1, jord2, n1, nordmx
    real(kind=8) :: inst1, inst2, trans, tprev, valr(2)
    character(len=8) :: k8b, resu2, resu1
    character(len=16) :: type, oper, chter
    character(len=19) :: nomch, cham1, resu19
    integer :: iarg, nbcham, kch
!
! ----------------------------------------------------------------------
    call jemarq()
!
!
!     -- ALLOCATION DE RESU2
!     -- CALCUL DE JORD2
!     --------------------------------
    call getres(resu2, type, oper)
    call getfac('ASSE', nbfac)
!
!     -- LA RE-ENTRANCE EST INTERDITE:
    call jeexin(resu2//'           .DESC', iret)
    call assert(iret.eq.0)
!
!     -- ON COMPTE LE NOMBRE MAX. DE NUMEROS D'ORDRE DE LA
!        SD_RESULTAT :
    nordmx=0
    do 101 iocc = 1, nbfac
        call getvid('ASSE', 'RESULTAT', iocc, iarg, 1,&
                    resu1, n1)
        resu19=resu1
        call jelira(resu19//'.ORDR', 'LONUTI', nbord1, k8b)
        nordmx=nordmx+nbord1
101  end do
!
!
!     -- ALLOCATION DE LA SD_RESULTAT :
    call rscrsd('G', resu2, type, nordmx)
    resu19=resu2
    call jeveuo(resu19//'.ORDR', 'L', jord2)
!
!
!     BOUCLE SUR LES OCCURRENCES DE ASSE :
!     ------------------------------------
    iord2=0
    tprev=-1.d300
    do 100 iocc = 1, nbfac
        call getvr8('ASSE', 'TRANSLATION', iocc, iarg, 1,&
                    trans, n1)
        call getvid('ASSE', 'RESULTAT', iocc, iarg, 1,&
                    resu1, n1)
        resu19=resu1
        call jelira(resu19//'.ORDR', 'LONUTI', nbord1, k8b)
        call jeveuo(resu19//'.ORDR', 'L', jord1)
        call jelira(resu19//'.DESC', 'NOMUTI', nbcham, k8b)
!
!       BOUCLE SUR LES CHAMPS 'TEMP' DE RESU1 ET RECOPIE DANS RESU2:
!       -----------------------------------------------------------
        do 110, kord1=1,nbord1
        iord1 = zi(jord1-1+kord1)
        iord2 = iord2 + 1
!
!         -- STOCKAGE DE L'INSTANT :
        call rsadpa(resu1, 'L', 1, 'INST', iord1,&
                    0, iad, k8b)
        inst1=zr(iad)
!         -- ON VERIFIE QUE LES INSTANTS SONT CROISSANTS :
        inst2=inst1+trans
        if (inst2 .lt. tprev) then
            valr(1)=tprev
            valr(2)=inst2
            call u2mesr('F', 'CALCULEL4_21', 2, valr)
        else if (inst2.eq.tprev) then
!           -- SI UN INSTANT EST TROUVE PLUSIEURS FOIS, ON ECRASE :
            call u2mesr('I', 'CALCULEL4_22', 1, inst2)
            iord2=iord2-1
        endif
        tprev=inst2
!
        call rsadpa(resu2, 'E', 1, 'INST', iord2,&
                    0, iad, k8b)
        zr(iad)=inst2
!
        do 115 kch = 1, nbcham
            call jenuno(jexnum(resu19//'.DESC', kch), chter)
!
!           1- RECUPERATION DU CHAMP : CHAM1
            call rsexch(' ', resu1, chter, iord1, cham1,&
                        iret)
            if (iret .ne. 0) goto 115
!
!           2- STOCKAGE DE CHAM1 :
            call rsexch(' ', resu2, chter, iord2, nomch,&
                        iret)
            call assert(iret.eq.0.or.iret.eq.100)
            call copisd('CHAMP_GD', 'G', cham1, nomch)
            call rsnoch(resu2, chter, iord2)
!
115      continue
110      continue
100  end do
!
!
!     -- EVENTUELLEMENT, IL FAUT DETRUIRE LES PROF_CHNO INUTILES
!        (A CAUSE DES INSTANTS MULTIPLES) :
    call rsmena(resu2)
!
!
    call jedema()
end subroutine
