subroutine refe81(nomres, basmod, raid, mass, amor,&
                  mailla)
    implicit none
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
!  P. RICHARD     DATE 13/07/90
!-----------------------------------------------------------------------
!  BUT : < CREATION DU REFE ET DU DESC POUR OP0081 >
!
!        - RECUPERER LES NOMS UTILISATEUR DES CONCEPTS ASSOCIES AUX
!          MATRICES ASSEMBLEES ET BASE MODALE CONSIDEREES
!        - EFFECTUER QUELQUES CONTROLES ET DETERMINER
!          OPTION DE CALCUL MATRICES PROJETEES
!-----------------------------------------------------------------------
!
! NOMRES /I/ : NOM UTILISATEUR DU RESULTAT
! BASMOD /O/ : NOM UT DE LA BASE MODALE DE PROJECTION
! RAID   /O/ : NOM UT DE LA MATRICE RAIDEUR A PROJETER
! MASS   /O/ : NOM UT DE LA MATRICE DE MASSE A PROJETER
! AMOR   /O/ : NOM UT DE LA MATRICE D'AMORTISSEMENT A PROJETER
! MAILLA /O/ : NOM UT DU MAILLAGE EN AMONT
!
!
!
!
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=24) :: valk(2), typbas
    character(len=8) :: nomres, mailla, basmod, maillb, bl8, lintf
    character(len=14) :: numddl, numbis, numter
    character(len=19) :: raid, mass, amor
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iadref, ioc, iret, lddesc, lldesc
    integer :: llref, nbval
!-----------------------------------------------------------------------
    data bl8         /'        '/
!-----------------------------------------------------------------------
    call jemarq()
!
! --- RECUPERATION BASE MODALE OBLIGATOIRE
!
    call getvid(' ', 'BASE_MODALE', scal=basmod, nbret=nbval)
!
! --- RECUPERATION MATRICE RAIDEUR EN ARGUMENT
!
    raid = bl8
    call getvid(' ', 'MATR_RIGI', nbval=0, nbret=ioc)
    ioc = -ioc
    if (ioc .eq. 0) then
        call dismoi('REF_RIGI_PREM', basmod, 'RESU_DYNA', repk=raid, arret='C',&
                    ier=iret)
    else if (ioc .eq. 1) then
        call getvid(' ', 'MATR_RIGI', scal=raid, nbret=iret)
    else
        call utmess('F', 'ALGORITH14_14')
    endif
!
! --- RECUPERATION MATRICE MASSE EN ARGUMENT
!
    mass = bl8
    call getvid(' ', 'MATR_MASS', nbval=0, nbret=ioc)
    ioc = -ioc
    if (ioc .eq. 0) then
        call dismoi('REF_MASS_PREM', basmod, 'RESU_DYNA', repk=mass, arret='C',&
                    ier=iret)
    else if (ioc .eq. 1) then
        call getvid(' ', 'MATR_MASS', scal=mass, nbret=iret)
    else
        call utmess('F', 'ALGORITH14_15')
    endif
!
! --- RECUPERATION MATRICE AMORTISSEMENT EN ARGUMENT
!
    amor = bl8
    call getvid(' ', 'MATR_AMOR', nbval=0, nbret=ioc)
    ioc = -ioc
    if (ioc .eq. 0) then
        call dismoi('REF_AMOR_PREM', basmod, 'RESU_DYNA', repk=amor, arret='C',&
                    ier=iret)
    else if (ioc .eq. 1) then
        call getvid(' ', 'MATR_AMOR', scal=amor, nbret=iret)
    else
        call utmess('F', 'ALGORITH14_16')
    endif
!
    call dismoi('NUME_DDL', basmod, 'RESU_DYNA', repk=numddl, arret='C',&
                ier=iret)
    call dismoi('REF_INTD_PREM', basmod, 'RESU_DYNA', repk=lintf, arret='C',&
                ier=iret)
    call dismoi('TYPE_BASE', basmod, 'RESU_DYNA', repk=typbas, arret='C',&
                ier=iret)
!
!
! --- RECUPERATION MAILLAGE
!
    if (lintf .ne. bl8) then
        call jeveuo(lintf//'.IDC_REFE', 'L', llref)
        mailla = zk24(llref)
    else
        call dismoi('NOM_MAILLA', numddl, 'NUME_DDL', repk=mailla)
    endif
!
! --- TRAITEMENT COHERENCE MATRICE ASSEMBLEES
!     SI MASSF ET RAIDF NON BL8
!
    if (raid .eq. bl8 .or. mass .eq. bl8) goto 10
!
    call dismoi('NOM_NUME_DDL', raid, 'MATR_ASSE', repk=numddl)
    call dismoi('NOM_MAILLA', numddl, 'NUME_DDL', repk=maillb)
    call dismoi('NOM_NUME_DDL', mass, 'MATR_ASSE', repk=numbis)
!
    if (amor .ne. bl8) then
        call dismoi('NOM_NUME_DDL', amor, 'MATR_ASSE', repk=numter)
    endif
!
! --- CONTROLE DE LA COHERENCE DES MATRICES ASSEMBLEES
!
    if (numddl .ne. numbis) then
        valk(1) = mass
        valk(2) = raid
        call utmess('F', 'ALGORITH14_21', nk=2, valk=valk)
    endif
!
    if (amor .ne. bl8) then
        if (numddl .ne. numter) then
            valk(1) = amor
            valk(2) = raid
            call utmess('F', 'ALGORITH14_22', nk=2, valk=valk)
        endif
    endif
!
    if (mailla .ne. maillb) then
        valk(1) = maillb
        valk(2) = mailla
        call utmess('F', 'ALGORITH14_23', nk=2, valk=valk)
    endif
!
 10 continue
!
! --- REMPLISSAGE DU .REFE
!
    call wkvect(nomres//'.MAEL_REFE', 'G V K24', 2, iadref)
    zk24(iadref) = basmod
    zk24(iadref+1) = mailla
!
! --- REMPLISSAGE DU .DESC
!
    call wkvect(nomres//'.MAEL_DESC', 'G V I', 3, lddesc)
    if (lintf .ne. bl8) then
        call jeveuo(lintf//'.IDC_DESC', 'L', lldesc)
        zi(lddesc) = zi(lldesc+1)
        zi(lddesc+1) = zi(lldesc+2)
        zi(lddesc+2) = zi(lldesc+3)
    endif
!
    call jedema()
end subroutine
