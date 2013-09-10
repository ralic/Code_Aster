subroutine refe99(nomres)
    implicit none
#include "jeveux.h"
!
#include "asterc/getfac.h"
#include "asterc/getvid.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/refdaj.h"
#include "asterfort/refdcp.h"
#include "asterfort/rsorac.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: nomres
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     BUT:
!       RECUPERER LES NOMS UTILISATEUR DES CONCEPTS ASSOCIES AUX
!       MATRICES ASSEMBLEES CONSIDEREES - EFFECTUER QUELQUES CONTROLES
!       CREER LE .REFD
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   NOMRES   : NOM DE LA SD_RESULTAT
!
! ......................................................................
!
!
!
!
    character(len=6) :: nompro
    parameter (nompro='REFE99')
!
    integer :: i, ioc1, ioc3, ioc4, ioc5, ier, ibid, ibmo, imint, inmax
    integer :: ltmome, nbg, nbmome, ltnbmo, ltnbmax, nbli, nbmax, vali(2)
    integer :: nbtot, nbold, nbmod1, nbmod2, nbmout, nbmodo
!
    real(kind=8) :: rbid
    complex(kind=8) :: cbid
!
    character(len=8) :: k8b, resul1, resul2, momeca, mostat
    character(len=19) :: numddl, numbis
    character(len=24) :: raid, mass, intf, intfb, amor, concep(3), valk(4), kbid
!
    logical :: noseul
!
!-----------------------------------------------------------------------
!
    call jemarq()
    numddl = ' '
    raid = ' '
    mass = ' '
    amor = ' '
!
! --- DETERMINATION DU TYPE DE BASE
!
    call getfac('CLASSIQUE', ioc1)
    call getfac('RITZ', ioc3)
    call getfac('DIAG_MASS', ioc4)
    call getfac('ORTHO_BASE', ioc5)
!
! --- CAS CLASSIQUE
!
    if (ioc1 .gt. 0) then
        numbis =' '
        call getvid('CLASSIQUE', 'INTERF_DYNA', 1, ibid, 1, intf, ier)
        call dismoi('F', 'NOM_NUME_DDL', intf, 'INTERF_DYNA', ibid, numddl, ier)
        call getvid('CLASSIQUE', 'MODE_MECA', 1, ibid, 0, k8b, nbmome)
        nbmome = -nbmome

        call wkvect('&&REFE99.LIST.MODE_MECA', 'V V K8', nbmome, ltmome)
        call wkvect('&&REFE99.LIST.NBMOD'    , 'V V I' , nbmome, ltnbmo)
        call wkvect('&&REFE99.LIST.NBMODMAX' , 'V V I' , nbmome, ltnbmax)

        call getvid('CLASSIQUE', 'MODE_MECA', 1, ibid, nbmome, zk8(ltmome), ibid)

        call getvis('CLASSIQUE', 'NMAX_MODE', 1, ibid, 0, ibid, nbli)
        nbli = -nbli
        if (nbli .eq. 0) then
!           Select all modes from each modal base 
            do 2 i = 1,nbmome
                zi(ltnbmax+i-1) = 9999
 2          continue
        else if (nbli .eq. 1) then
!           Apply the single NMAX_MODE criterion to all of the modal base
            call getvis('CLASSIQUE', 'NMAX_MODE', 1, ibid, 1, nbmax, ier)
            do 3 i = 1,nbmome
                zi(ltnbmax+i-1) = nbmax
 3          continue
        else if (nbli .eq. nbmome) then
!           Use the NMAX_MODE criteria, defined for each modal base separately
            call getvis('CLASSIQUE', 'NMAX_MODE', 1, ibid, nbmome, zi(ltnbmax), ier)
        else 
!           Incoherence in the input data
            vali(1) = nbmome
            vali(2) = nbli
            call u2mesi('F', 'ALGORITH14_31', 2, vali)
        endif
!
        do 10 i = 1, nbmome
            momeca = zk8(ltmome-1+i)
            call dismoi('F', 'REF_RIGI_PREM', momeca, 'RESU_DYNA', ibid, raid, ier)
            call dismoi('F', 'REF_MASS_PREM', momeca, 'RESU_DYNA', ibid, mass, ier)
            call dismoi('F', 'REF_AMOR_PREM', momeca, 'RESU_DYNA', ibid, amor, ier)
            call dismoi('F', 'NUME_DDL'     , momeca, 'RESU_DYNA', ibid, numbis, ier)
!           Check for nume_ddl coherence of each mode_meca with that of interf_dyna
            if (numbis(1:14) .ne. numddl(1:14)) then
                valk (1) = momeca
                valk (2) = numbis(1:8)
                valk (3) = intf
                valk (4) = numddl(1:8)
                call u2mesk('F', 'ALGORITH14_24', 4, valk)
            endif
!           Determine the real number of modes to recuperate from each base
            call rsorac(momeca, 'LONUTI', ibid, rbid, kbid, cbid, rbid, &
                        'ABSOLU', nbmodo, 1, ibid)
            nbmout = zi(ltnbmax+i-1)
            if (nbmodo .lt. nbmout) then
                call u2mess('I', 'ALGORITH15_92')
                valk = momeca
                call u2mesg('I', 'ALGORITH15_93', 1, valk, 0, 0, 0, 0.d0)
                vali = nbmodo
                call u2mesg('I', 'ALGORITH15_94', 0, ' ', 1, vali, 0, 0.d0)
            else
                nbmodo = nbmout
            endif
            zi(ltnbmo+i-1) = nbmodo
!           Add a reference in the dynamic result data structure
            concep(1) = raid
            concep(2) = mass
            concep(3) = amor
            call refdaj('F', nomres, nbmodo, numddl, 'DYNAMIQUE', concep, ier)
            concep(1) = intf
            call refdaj('F', nomres, 0, numddl, 'INTERF_DYNA', concep, ier)
10      continue
!
    endif
!
! --- CAS RITZ
!
    if (ioc3 .gt. 0) then

        noseul=.false.
        call getvid('RITZ', 'MODE_MECA', 1, ibid, 999, k8b, nbg)
        call getvid('RITZ', 'MODE_INTF', 2, ibid, 0  , k8b, ier)
        if ((ier.gt.0) .or. (nbg.gt.1)) noseul=.true.

!       Reference numbering is required in case more than one modal base is given
        call getvid('    ', 'NUME_REF', 1, ibid, 1, numddl, ier)
        if ((ier.eq.0) .and. noseul) call u2mess('E', 'ALGORITH17_9')
!
        intf = ' '
        call getvid('  ', 'INTERF_DYNA', 1, ibid, 0, k8b, ier)
        if (ier .lt. 0) call getvid('  ', 'INTERF_DYNA', 1, ibid, 1, intf, ier)
!
        call getvid('RITZ', 'BASE_MODALE', 1, ibid, 1, resul1, ibmo)
        call getvid('RITZ', 'MODE_INTF', 2, ibid, 1, resul2, imint)       
!
!       BASE_MODALE kw treatment (with INTERF_DYNA and MODE_INTF on the 2nd occurence)
        if (ibmo .ne. 0) then
            call refdcp(resul1, nomres)
            call dismoi('F', 'NB_MODES_TOT', resul1, 'RESULTAT', nbmod1, k8b, ier)
        else
!           MODE_MECA kw treatment, similar to what is done for the "classique" case
            call getvid('RITZ', 'MODE_MECA', 1, ibid, 0, k8b, nbmome)
            nbmome = -nbmome

            call wkvect('&&REFE99.LIST.MODE_MECA', 'V V K8', nbmome, ltmome)
            call wkvect('&&REFE99.LIST.NBMOD'    , 'V V I' , nbmome, ltnbmo)
            call wkvect('&&REFE99.LIST.NBMODMAX' , 'V V I' , nbmome, ltnbmax)

            call getvid('RITZ', 'MODE_MECA', 1, ibid, nbmome, zk8(ltmome), ibid)

            call getvis('RITZ', 'NMAX_MODE', 1, ibid, 0, ibid, nbli)
            nbli = -nbli
            if (nbli .eq. 0) then
!               Select all modes from each modal base 
                do 12 i = 1,nbmome
                    zi(ltnbmax+i-1) = 9999
12              continue
            else if (nbli .eq. 1) then
!               Apply the single NMAX_MODE criterion to all of the modal base
                call getvis('RITZ', 'NMAX_MODE', 1, ibid, 1, nbmax, ibid)
                do 13 i = 1,nbmome
                    zi(ltnbmax+i-1) = nbmax
13              continue
            else if (nbli .eq. nbmome) then
!               Use the NMAX_MODE criteria, defined for each modal base separately
                call getvis('RITZ', 'NMAX_MODE', 1, ibid, nbmome, zi(ltnbmax), ibid)
            else 
!               Incoherence in the input data
                vali(1) = nbmome
                vali(2) = nbli
                call u2mesi('F', 'ALGORITH14_31', 2, vali)
            endif
!
            nbmod1 = 0
            do 20 i = 1, nbmome
                momeca = zk8(ltmome-1+i)
                call dismoi('F', 'REF_RIGI_PREM', momeca, 'RESU_DYNA', ibid, raid, ier)
                call dismoi('F', 'REF_MASS_PREM', momeca, 'RESU_DYNA', ibid, mass, ier)
                call dismoi('F', 'REF_AMOR_PREM', momeca, 'RESU_DYNA', ibid, amor, ier)
                call dismoi('F', 'NUME_DDL'     , momeca, 'RESU_DYNA', ibid, numbis, ier)
                if (numddl .eq. ' ') numddl = numbis
!               Determine the real number of modes to recuperate from each base
                call rsorac(momeca, 'LONUTI', ibid, rbid, kbid, cbid, rbid, &
                            'ABSOLU', nbmodo, 1, ibid)
                nbmout = zi(ltnbmax+i-1)
                if (nbmodo .lt. nbmout) then
                    call u2mess('I', 'ALGORITH15_92')
                    valk = momeca
                    call u2mesg('I', 'ALGORITH15_93', 1, valk, 0, 0, 0, 0.d0)
                    vali = nbmodo
                    call u2mesg('I', 'ALGORITH15_94', 0, ' ', 1, vali, 0, 0.d0)
                else
                    nbmodo = nbmout
                endif
                zi(ltnbmo+i-1) = nbmodo
                nbmod1 = nbmod1 + nbmodo
!               Add a reference in the dynamic result data structure
                concep(1) = raid
                concep(2) = mass
                concep(3) = amor
                call refdaj('F', nomres, nbmodo, numddl, 'DYNAMIQUE', concep, ier)
20          continue
        endif
        if (imint .gt. 0) then
!           Treating the MODE_INTF kw (2nd RITZ entry) for the static modes
!           Maximum number of static modes to extract : nbmod2
            call getvis('RITZ', 'NMAX_MODE', 2, ibid, 1, nbmod2, inmax)
!           Number of modes that actually exist in the static base : nbold
            call rsorac(resul2, 'LONUTI', ibid, rbid, k8b, cbid, rbid, & 
                        'ABSOLU', nbold, 1, ibid)
            if (inmax .eq. 0) then
                nbmod2 = nbold
            else
                nbmod2 = min(nbmod2,nbold)
            endif
!
            concep(1) = resul2
            call refdaj('F', nomres, nbmod2, numddl, 'INTERF_STAT', concep, ier)
        else 
            nbmod2 = 0
        endif
!
        nbtot = nbmod1 + nbmod2
        if (nbtot .le. 0) then
            call u2mess('F', 'ALGORITH14_50')
        endif
!
        call dismoi('C', 'REF_INTD_DERN', nomres, 'RESU_DYNA', ibid, intfb, ier)
        if ((intf .ne. ' ') .and. (intf .ne. intfb)) then
            concep(1) = intf
            call refdaj('F', nomres, 0, numddl, 'INTERF_DYNA', concep, ier)
        endif
    endif
!
! --- DIAGONALISATION DE LA MATRICE DE MASSE
!
    if (ioc4 .gt. 0) then
        intf = ' '
        call getvid('DIAG_MASS', 'MODE_MECA', 1, ibid, 1, momeca, ibid)
!
        call dismoi('F', 'REF_RIGI_PREM', momeca, 'RESU_DYNA', ibid, raid, ier)
        call dismoi('F', 'REF_MASS_PREM', momeca, 'RESU_DYNA', ibid, mass, ier)
        call dismoi('F', 'REF_AMOR_PREM', momeca, 'RESU_DYNA', ibid, amor, ier)
        call dismoi('F', 'NOM_NUME_DDL' , mass, 'MATR_ASSE', ibid, numddl, ier)
        call dismoi('F', 'NB_MODES_TOT', momeca, 'RESULTAT', nbmod1, k8b, ier)
        concep(1) = raid
        concep(2) = mass
        concep(3) = amor
        call refdaj('F', nomres, nbmod1, numddl, 'DYNAMIQUE', concep, ier)
!
        call getvid('DIAG_MASS', 'MODE_STAT', 1, ibid, 1, mostat, ibid)
        call dismoi('F', 'NB_MODES_TOT', mostat, 'RESULTAT', nbmod2, k8b, ier)
        concep(1) = mostat
!       Note that it is volontary to save the numbering associated to the dynamic modes
!       because later on we call copmod upon the static modes, modifying their nume_ddl
        call refdaj('F', nomres, nbmod2, numddl, 'INTERF_STAT', concep, ier)
    endif
!
! --- CAS ORTHO_BASE
!
    if (ioc5 .gt. 0) then
        call getvid('ORTHO_BASE', 'BASE', 1, ibid, 1, resul1, ibid)
        call refdcp(resul1,nomres)
    endif
!
    call jedema()
!
end subroutine
