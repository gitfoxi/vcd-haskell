
-- TODO:
--  Decompress: .gz .bz2 .xz
--  Compress: .bz2 using pbzip2; fallback to .gz
--  Warn if they don't use .bz2 for input or output
--  Warn if < 48 cores
--  Pins you want to keep (instead of just pins you want to toss)
--  Also filter multiple consecutive timestamp lines, keeping only the last
--
--   #100
--   #200
--
--   becomes
--
--   #200
--
--  YAML config

{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent -- (getNumCapabilities, Chan, writeChan, newChan, forkIO, getChanContents)
import           Control.Concurrent.MVar
import           Control.Monad (when, replicateM)
import           Control.Parallel.Strategies
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Short (fromShort, toShort, ShortByteString)
import           Data.List (transpose)
import qualified Data.Traversable as Traversable
-- import qualified Data.Set as Set
import qualified Data.HashSet as Set
import           System.Environment (getArgs)
import           System.IO (stderr, hPutStrLn)
import           System.IO.Unsafe

import Vcd

import Debug.Trace

testParseSignal s = do
    let sig = parse parseSignal s
    case sig of
        Fail unconsumed contexts message ->
            error $ "Parse failed: "
                 ++ message
                 ++ "\n---- Contexts ----\n"
                 ++ unlines contexts
                 ++ "\n------------------\n"
                 ++ "\nUnconsumed:\n" ++ Prelude.take 200 (B.unpack unconsumed)
                 ++ "\n...\n"
        Done theRest sigs -> do
            putStrLn "Parse Succeeds"
            print sigs
            putStrLn "Leftover"
            print theRest

sigsToKeep :: Set.HashSet B.ByteString
sigsToKeep = Set.fromList
  [ "usb0_dm"
  ,"usb0_dp"
  ,"usb0_ssrx_m"
  ,"usb0_ssrx_p"
  ,"usb0_sstx_m"
  ,"usb0_sstx_p"
  ,"usb0_analog_test"
  ,"usb0_resref"
  ,"usb1_dm"
  ,"usb1_dp"
  ,"usb1_ssrx_m"
  ,"usb1_ssrx_p"
  ,"usb1_sstx_m"
  ,"usb1_sstx_p"
  ,"usb1_analog_test"
  ,"usb1_resref"
  ,"usb0_xi_nc"
  ,"usb0_xo_nc"
  ,"usb0_vbus_nc"
  ,"usb1_xi_nc"
  ,"usb1_xo_nc"
  ,"usb1_vbus_nc"
  ,"qlm0_txn"
  ,"qlm0_txn"
  ,"qlm0_txn"
  ,"qlm0_txn"
  ,"qlm0_txp"
  ,"qlm0_txp"
  ,"qlm0_txp"
  ,"qlm0_txp"
  ,"qlm1_txn"
  ,"qlm1_txn"
  ,"qlm1_txn"
  ,"qlm1_txn"
  ,"qlm1_txp"
  ,"qlm1_txp"
  ,"qlm1_txp"
  ,"qlm1_txp"
  ,"qlm2_txn"
  ,"qlm2_txn"
  ,"qlm2_txn"
  ,"qlm2_txn"
  ,"qlm2_txp"
  ,"qlm2_txp"
  ,"qlm2_txp"
  ,"qlm2_txp"
  ,"qlm3_txn"
  ,"qlm3_txn"
  ,"qlm3_txn"
  ,"qlm3_txn"
  ,"qlm3_txp"
  ,"qlm3_txp"
  ,"qlm3_txp"
  ,"qlm3_txp"
  ,"qlm4_txn"
  ,"qlm4_txn"
  ,"qlm4_txn"
  ,"qlm4_txn"
  ,"qlm4_txp"
  ,"qlm4_txp"
  ,"qlm4_txp"
  ,"qlm4_txp"
  ,"qlm5_txn"
  ,"qlm5_txn"
  ,"qlm5_txn"
  ,"qlm5_txn"
  ,"qlm5_txp"
  ,"qlm5_txp"
  ,"qlm5_txp"
  ,"qlm5_txp"
  ,"qlm6_txn"
  ,"qlm6_txn"
  ,"qlm6_txn"
  ,"qlm6_txn"
  ,"qlm6_txp"
  ,"qlm6_txp"
  ,"qlm6_txp"
  ,"qlm6_txp"
  ,"qlm7_txn"
  ,"qlm7_txn"
  ,"qlm7_txn"
  ,"qlm7_txn"
  ,"qlm7_txp"
  ,"qlm7_txp"
  ,"qlm7_txp"
  ,"qlm7_txp"
  ,"qlm0_amon"
  ,"qlm0_dmon"
  ,"qlm0_dmonb"
  ,"qlm1_amon"
  ,"qlm1_dmon"
  ,"qlm1_dmonb"
  ,"qlm2_amon"
  ,"qlm2_dmon"
  ,"qlm2_dmonb"
  ,"qlm3_amon"
  ,"qlm3_dmon"
  ,"qlm3_dmonb"
  ,"qlm4_amon"
  ,"qlm4_dmon"
  ,"qlm4_dmonb"
  ,"qlm5_amon"
  ,"qlm5_dmon"
  ,"qlm5_dmonb"
  ,"qlm6_amon"
  ,"qlm6_dmon"
  ,"qlm6_dmonb"
  ,"qlm7_amon"
  ,"qlm7_dmon"
  ,"qlm7_dmonb"
  ,"oci0_amon"
  ,"oci0_dmon"
  ,"oci0_dmonb"
  ,"oci0_txn"
  ,"oci0_txn"
  ,"oci0_txn"
  ,"oci0_txn"
  ,"oci0_txp"
  ,"oci0_txp"
  ,"oci0_txp"
  ,"oci0_txp"
  ,"oci1_amon"
  ,"oci1_dmon"
  ,"oci1_dmonb"
  ,"oci1_txn"
  ,"oci1_txn"
  ,"oci1_txn"
  ,"oci1_txn"
  ,"oci1_txp"
  ,"oci1_txp"
  ,"oci1_txp"
  ,"oci1_txp"
  ,"oci2_amon"
  ,"oci2_dmon"
  ,"oci2_dmonb"
  ,"oci2_txn"
  ,"oci2_txn"
  ,"oci2_txn"
  ,"oci2_txn"
  ,"oci2_txp"
  ,"oci2_txp"
  ,"oci2_txp"
  ,"oci2_txp"
  ,"oci3_amon"
  ,"oci3_dmon"
  ,"oci3_dmonb"
  ,"oci3_txn"
  ,"oci3_txn"
  ,"oci3_txn"
  ,"oci3_txn"
  ,"oci3_txp"
  ,"oci3_txp"
  ,"oci3_txp"
  ,"oci3_txp"
  ,"oci4_amon"
  ,"oci4_dmon"
  ,"oci4_dmonb"
  ,"oci4_txn"
  ,"oci4_txn"
  ,"oci4_txn"
  ,"oci4_txn"
  ,"oci4_txp"
  ,"oci4_txp"
  ,"oci4_txp"
  ,"oci4_txp"
  ,"oci5_amon"
  ,"oci5_dmon"
  ,"oci5_dmonb"
  ,"oci5_txn"
  ,"oci5_txn"
  ,"oci5_txn"
  ,"oci5_txn"
  ,"oci5_txp"
  ,"oci5_txp"
  ,"oci5_txp"
  ,"oci5_txp"
  ,"qlm0_rxn"
  ,"qlm0_rxn"
  ,"qlm0_rxn"
  ,"qlm0_rxn"
  ,"qlm0_rxp"
  ,"qlm0_rxp"
  ,"qlm0_rxp"
  ,"qlm0_rxp"
  ,"qlm0_vss"
  ,"qlm1_rxn"
  ,"qlm1_rxn"
  ,"qlm1_rxn"
  ,"qlm1_rxn"
  ,"qlm1_rxp"
  ,"qlm1_rxp"
  ,"qlm1_rxp"
  ,"qlm1_rxp"
  ,"qlm1_vss"
  ,"qlm2_rxn"
  ,"qlm2_rxn"
  ,"qlm2_rxn"
  ,"qlm2_rxn"
  ,"qlm2_rxp"
  ,"qlm2_rxp"
  ,"qlm2_rxp"
  ,"qlm2_rxp"
  ,"qlm2_vss"
  ,"qlm3_rxn"
  ,"qlm3_rxn"
  ,"qlm3_rxn"
  ,"qlm3_rxn"
  ,"qlm3_rxp"
  ,"qlm3_rxp"
  ,"qlm3_rxp"
  ,"qlm3_rxp"
  ,"qlm3_vss"
  ,"qlm4_rxn"
  ,"qlm4_rxn"
  ,"qlm4_rxn"
  ,"qlm4_rxn"
  ,"qlm4_rxp"
  ,"qlm4_rxp"
  ,"qlm4_rxp"
  ,"qlm4_rxp"
  ,"qlm4_vss"
  ,"qlm0_rbias"
  ,"qlm0_vdda"
  ,"qlm0_vddhv"
  ,"qlm1_rbias"
  ,"qlm1_vdda"
  ,"qlm1_vddhv"
  ,"qlm2_rbias"
  ,"qlm2_vdda"
  ,"qlm2_vddhv"
  ,"qlm3_rbias"
  ,"qlm3_vdda"
  ,"qlm3_vddhv"
  ,"qlm4_rbias"
  ,"qlm4_vdda"
  ,"qlm4_vddhv"
  ,"qlm5_rbias"
  ,"qlm5_rxn"
  ,"qlm5_rxn"
  ,"qlm5_rxn"
  ,"qlm5_rxn"
  ,"qlm5_rxp"
  ,"qlm5_rxp"
  ,"qlm5_rxp"
  ,"qlm5_rxp"
  ,"qlm5_vdda"
  ,"qlm5_vddhv"
  ,"qlm5_vss"
  ,"qlm6_rbias"
  ,"qlm6_rxn"
  ,"qlm6_rxn"
  ,"qlm6_rxn"
  ,"qlm6_rxn"
  ,"qlm6_rxp"
  ,"qlm6_rxp"
  ,"qlm6_rxp"
  ,"qlm6_rxp"
  ,"qlm6_vdda"
  ,"qlm6_vddhv"
  ,"qlm6_vss"
  ,"qlm7_rbias"
  ,"qlm7_rxn"
  ,"qlm7_rxn"
  ,"qlm7_rxn"
  ,"qlm7_rxn"
  ,"qlm7_rxp"
  ,"qlm7_rxp"
  ,"qlm7_rxp"
  ,"qlm7_rxp"
  ,"qlm7_vdda"
  ,"qlm7_vddhv"
  ,"qlm7_vss"
  ,"oci0_rxn"
  ,"oci0_rxn"
  ,"oci0_rxn"
  ,"oci0_rxn"
  ,"oci0_rxp"
  ,"oci0_rxp"
  ,"oci0_rxp"
  ,"oci0_rxp"
  ,"oci0_vss"
  ,"oci1_rxn"
  ,"oci1_rxn"
  ,"oci1_rxn"
  ,"oci1_rxn"
  ,"oci1_rxp"
  ,"oci1_rxp"
  ,"oci1_rxp"
  ,"oci1_rxp"
  ,"oci1_vss"
  ,"oci2_rxn"
  ,"oci2_rxn"
  ,"oci2_rxn"
  ,"oci2_rxn"
  ,"oci2_rxp"
  ,"oci2_rxp"
  ,"oci2_rxp"
  ,"oci2_rxp"
  ,"oci2_vss"
  ,"oci3_rxn"
  ,"oci3_rxn"
  ,"oci3_rxn"
  ,"oci3_rxn"
  ,"oci3_rxp"
  ,"oci3_rxp"
  ,"oci3_rxp"
  ,"oci3_rxp"
  ,"oci3_vss"
  ,"oci4_rxn"
  ,"oci4_rxn"
  ,"oci4_rxn"
  ,"oci4_rxn"
  ,"oci4_rxp"
  ,"oci4_rxp"
  ,"oci4_rxp"
  ,"oci4_rxp"
  ,"oci4_vss"
  ,"oci5_rxn"
  ,"oci5_rxn"
  ,"oci5_rxn"
  ,"oci5_rxn"
  ,"oci5_rxp"
  ,"oci5_rxp"
  ,"oci5_rxp"
  ,"oci5_rxp"
  ,"oci5_vss"
  ,"oci0_rbias"
  ,"oci0_vdda"
  ,"oci0_vddhv"
  ,"oci1_rbias"
  ,"oci1_vdda"
  ,"oci1_vddhv"
  ,"oci2_rbias"
  ,"oci2_vdda"
  ,"oci2_vddhv"
  ,"oci3_rbias"
  ,"oci3_vdda"
  ,"oci3_vddhv"
  ,"oci4_rbias"
  ,"oci4_vdda"
  ,"oci4_vddhv"
  ,"oci5_rbias"
  ,"oci5_vdda"
  ,"oci5_vddhv"
  ,"efuse_en_n"
  ,"jtg_usb_trst_n"
  ,"ncsi_crs_dv"
  ,"ncsi_rx_er"
  ,"ncsi_rxd"
  ,"ncsi_rxd"
  ,"ncsi_tx_en"
  ,"ncsi_txd"
  ,"ncsi_txd"
  ,"perst4_n"
  ,"perst5_n"
  ,"jtg_dap_trst_n"
  ,"spi_cs"
  ,"spi_cs"
  ,"spi_miso"
  ,"spi_mosi"
  ,"tws2_scl"
  ,"tws2_sda"
  ,"tws3_scl"
  ,"tws3_sda"
  ,"tws4_scl"
  ,"tws4_sda"
  ,"tws5_scl"
  ,"tws5_sda"
  ,"atest_west"
  ,"ocic_vdd"
  ,"ocic_vss"
  ,"atest_east"
  ,"oci2_lnk1"
  ,"oci3_lnk1"
  ,"oci_fixed_node"
  ,"oci_node_id"
  ,"oci_node_id"
  ,"oci_spd"
  ,"oci_spd"
  ,"oci_spd"
  ,"oci_spd"
  ,"vdd_io_supply_select"
  ,"vdd_smi_supply_select"
  ,"vrm_i2_data"
  ,"ddr0_a"
  ,"ddr0_a"
  ,"ddr0_a"
  ,"ddr0_a"
  ,"ddr0_a"
  ,"ddr0_a"
  ,"ddr0_a"
  ,"ddr0_a"
  ,"ddr0_a"
  ,"ddr0_a"
  ,"ddr0_a"
  ,"ddr0_a"
  ,"ddr0_a"
  ,"ddr0_a"
  ,"ddr0_a14_bg0"
  ,"ddr0_a15_bg1"
  ,"ddr0_act_n"
  ,"ddr0_ba"
  ,"ddr0_ba"
  ,"ddr0_cas_n_a15"
  ,"ddr0_ck_h"
  ,"ddr0_ck_h"
  ,"ddr0_ck_h"
  ,"ddr0_ck_h"
  ,"ddr0_ck_l"
  ,"ddr0_ck_l"
  ,"ddr0_ck_l"
  ,"ddr0_ck_l"
  ,"ddr0_comp_dn"
  ,"ddr0_comp_up"
  ,"ddr0_cs0_n"
  ,"ddr0_cs0_n"
  ,"ddr0_cs1_n"
  ,"ddr0_cs1_n"
  ,"ddr0_dimm0_cke"
  ,"ddr0_dimm0_cke"
  ,"ddr0_dimm1_cke"
  ,"ddr0_dimm1_cke"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dq"
  ,"ddr0_dqs0_h"
  ,"ddr0_dqs0_h"
  ,"ddr0_dqs0_h"
  ,"ddr0_dqs0_h"
  ,"ddr0_dqs0_h"
  ,"ddr0_dqs0_h"
  ,"ddr0_dqs0_h"
  ,"ddr0_dqs0_h"
  ,"ddr0_dqs0_h"
  ,"ddr0_dqs0_l"
  ,"ddr0_dqs0_l"
  ,"ddr0_dqs0_l"
  ,"ddr0_dqs0_l"
  ,"ddr0_dqs0_l"
  ,"ddr0_dqs0_l"
  ,"ddr0_dqs0_l"
  ,"ddr0_dqs0_l"
  ,"ddr0_dqs0_l"
  ,"ddr0_dqs1_h"
  ,"ddr0_dqs1_h"
  ,"ddr0_dqs1_h"
  ,"ddr0_dqs1_h"
  ,"ddr0_dqs1_h"
  ,"ddr0_dqs1_h"
  ,"ddr0_dqs1_h"
  ,"ddr0_dqs1_h"
  ,"ddr0_dqs1_h"
  ,"ddr0_dqs1_l"
  ,"ddr0_dqs1_l"
  ,"ddr0_dqs1_l"
  ,"ddr0_dqs1_l"
  ,"ddr0_dqs1_l"
  ,"ddr0_dqs1_l"
  ,"ddr0_dqs1_l"
  ,"ddr0_dqs1_l"
  ,"ddr0_dqs1_l"
  ,"ddr0_error_alert_n"
  ,"ddr0_odt0"
  ,"ddr0_odt0"
  ,"ddr0_odt1"
  ,"ddr0_odt1"
  ,"ddr0_par"
  ,"ddr0_ras_n_a16"
  ,"ddr0_reset_n"
  ,"ddr0_we_n_a14"
  ,"ddr1_a"
  ,"ddr1_a"
  ,"ddr1_a"
  ,"ddr1_a"
  ,"ddr1_a"
  ,"ddr1_a"
  ,"ddr1_a"
  ,"ddr1_a"
  ,"ddr1_a"
  ,"ddr1_a"
  ,"ddr1_a"
  ,"ddr1_a"
  ,"ddr1_a"
  ,"ddr1_a"
  ,"ddr1_a14_bg0"
  ,"ddr1_a15_bg1"
  ,"ddr1_act_n"
  ,"ddr1_ba"
  ,"ddr1_ba"
  ,"ddr1_cas_n_a15"
  ,"ddr1_ck_h"
  ,"ddr1_ck_h"
  ,"ddr1_ck_h"
  ,"ddr1_ck_h"
  ,"ddr1_ck_l"
  ,"ddr1_ck_l"
  ,"ddr1_ck_l"
  ,"ddr1_ck_l"
  ,"ddr1_comp_dn"
  ,"ddr1_comp_up"
  ,"ddr1_cs0_n"
  ,"ddr1_cs0_n"
  ,"ddr1_cs1_n"
  ,"ddr1_cs1_n"
  ,"ddr1_dimm0_cke"
  ,"ddr1_dimm0_cke"
  ,"ddr1_dimm1_cke"
  ,"ddr1_dimm1_cke"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dq"
  ,"ddr1_dqs0_h"
  ,"ddr1_dqs0_h"
  ,"ddr1_dqs0_h"
  ,"ddr1_dqs0_h"
  ,"ddr1_dqs0_h"
  ,"ddr1_dqs0_h"
  ,"ddr1_dqs0_h"
  ,"ddr1_dqs0_h"
  ,"ddr1_dqs0_h"
  ,"ddr1_dqs0_l"
  ,"ddr1_dqs0_l"
  ,"ddr1_dqs0_l"
  ,"ddr1_dqs0_l"
  ,"ddr1_dqs0_l"
  ,"ddr1_dqs0_l"
  ,"ddr1_dqs0_l"
  ,"ddr1_dqs0_l"
  ,"ddr1_dqs0_l"
  ,"ddr1_dqs1_h"
  ,"ddr1_dqs1_h"
  ,"ddr1_dqs1_h"
  ,"ddr1_dqs1_h"
  ,"ddr1_dqs1_h"
  ,"ddr1_dqs1_h"
  ,"ddr1_dqs1_h"
  ,"ddr1_dqs1_h"
  ,"ddr1_dqs1_h"
  ,"ddr1_dqs1_l"
  ,"ddr1_dqs1_l"
  ,"ddr1_dqs1_l"
  ,"ddr1_dqs1_l"
  ,"ddr1_dqs1_l"
  ,"ddr1_dqs1_l"
  ,"ddr1_dqs1_l"
  ,"ddr1_dqs1_l"
  ,"ddr1_dqs1_l"
  ,"ddr1_error_alert_n"
  ,"ddr1_odt0"
  ,"ddr1_odt0"
  ,"ddr1_odt1"
  ,"ddr1_odt1"
  ,"ddr1_par"
  ,"ddr1_ras_n_a16"
  ,"ddr1_reset_n"
  ,"ddr1_we_n_a14"
  ,"ddr2_a"
  ,"ddr2_a"
  ,"ddr2_a"
  ,"ddr2_a"
  ,"ddr2_a"
  ,"ddr2_a"
  ,"ddr2_a"
  ,"ddr2_a"
  ,"ddr2_a"
  ,"ddr2_a"
  ,"ddr2_a"
  ,"ddr2_a"
  ,"ddr2_a"
  ,"ddr2_a"
  ,"ddr2_a14_bg0"
  ,"ddr2_a15_bg1"
  ,"ddr2_act_n"
  ,"ddr2_ba"
  ,"ddr2_ba"
  ,"ddr2_cas_n_a15"
  ,"ddr2_ck_h"
  ,"ddr2_ck_h"
  ,"ddr2_ck_h"
  ,"ddr2_ck_h"
  ,"ddr2_ck_l"
  ,"ddr2_ck_l"
  ,"ddr2_ck_l"
  ,"ddr2_ck_l"
  ,"ddr2_comp_dn"
  ,"ddr2_comp_up"
  ,"ddr2_cs0_n"
  ,"ddr2_cs0_n"
  ,"ddr2_cs1_n"
  ,"ddr2_cs1_n"
  ,"ddr2_dimm0_cke"
  ,"ddr2_dimm0_cke"
  ,"ddr2_dimm1_cke"
  ,"ddr2_dimm1_cke"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dq"
  ,"ddr2_dqs0_h"
  ,"ddr2_dqs0_h"
  ,"ddr2_dqs0_h"
  ,"ddr2_dqs0_h"
  ,"ddr2_dqs0_h"
  ,"ddr2_dqs0_h"
  ,"ddr2_dqs0_h"
  ,"ddr2_dqs0_h"
  ,"ddr2_dqs0_h"
  ,"ddr2_dqs0_l"
  ,"ddr2_dqs0_l"
  ,"ddr2_dqs0_l"
  ,"ddr2_dqs0_l"
  ,"ddr2_dqs0_l"
  ,"ddr2_dqs0_l"
  ,"ddr2_dqs0_l"
  ,"ddr2_dqs0_l"
  ,"ddr2_dqs0_l"
  ,"ddr2_dqs1_h"
  ,"ddr2_dqs1_h"
  ,"ddr2_dqs1_h"
  ,"ddr2_dqs1_h"
  ,"ddr2_dqs1_h"
  ,"ddr2_dqs1_h"
  ,"ddr2_dqs1_h"
  ,"ddr2_dqs1_h"
  ,"ddr2_dqs1_h"
  ,"ddr2_dqs1_l"
  ,"ddr2_dqs1_l"
  ,"ddr2_dqs1_l"
  ,"ddr2_dqs1_l"
  ,"ddr2_dqs1_l"
  ,"ddr2_dqs1_l"
  ,"ddr2_dqs1_l"
  ,"ddr2_dqs1_l"
  ,"ddr2_dqs1_l"
  ,"ddr2_error_alert_n"
  ,"ddr2_odt0"
  ,"ddr2_odt0"
  ,"ddr2_odt1"
  ,"ddr2_odt1"
  ,"ddr2_par"
  ,"ddr2_ras_n_a16"
  ,"ddr2_reset_n"
  ,"ddr2_we_n_a14"
  ,"ddr3_a"
  ,"ddr3_a"
  ,"ddr3_a"
  ,"ddr3_a"
  ,"ddr3_a"
  ,"ddr3_a"
  ,"ddr3_a"
  ,"ddr3_a"
  ,"ddr3_a"
  ,"ddr3_a"
  ,"ddr3_a"
  ,"ddr3_a"
  ,"ddr3_a"
  ,"ddr3_a"
  ,"ddr3_a14_bg0"
  ,"ddr3_a15_bg1"
  ,"ddr3_act_n"
  ,"ddr3_ba"
  ,"ddr3_ba"
  ,"ddr3_cas_n_a15"
  ,"ddr3_ck_h"
  ,"ddr3_ck_h"
  ,"ddr3_ck_h"
  ,"ddr3_ck_h"
  ,"ddr3_ck_l"
  ,"ddr3_ck_l"
  ,"ddr3_ck_l"
  ,"ddr3_ck_l"
  ,"ddr3_comp_dn"
  ,"ddr3_comp_up"
  ,"ddr3_cs0_n"
  ,"ddr3_cs0_n"
  ,"ddr3_cs1_n"
  ,"ddr3_cs1_n"
  ,"ddr3_dimm0_cke"
  ,"ddr3_dimm0_cke"
  ,"ddr3_dimm1_cke"
  ,"ddr3_dimm1_cke"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dq"
  ,"ddr3_dqs0_h"
  ,"ddr3_dqs0_h"
  ,"ddr3_dqs0_h"
  ,"ddr3_dqs0_h"
  ,"ddr3_dqs0_h"
  ,"ddr3_dqs0_h"
  ,"ddr3_dqs0_h"
  ,"ddr3_dqs0_h"
  ,"ddr3_dqs0_h"
  ,"ddr3_dqs0_l"
  ,"ddr3_dqs0_l"
  ,"ddr3_dqs0_l"
  ,"ddr3_dqs0_l"
  ,"ddr3_dqs0_l"
  ,"ddr3_dqs0_l"
  ,"ddr3_dqs0_l"
  ,"ddr3_dqs0_l"
  ,"ddr3_dqs0_l"
  ,"ddr3_dqs1_h"
  ,"ddr3_dqs1_h"
  ,"ddr3_dqs1_h"
  ,"ddr3_dqs1_h"
  ,"ddr3_dqs1_h"
  ,"ddr3_dqs1_h"
  ,"ddr3_dqs1_h"
  ,"ddr3_dqs1_h"
  ,"ddr3_dqs1_h"
  ,"ddr3_dqs1_l"
  ,"ddr3_dqs1_l"
  ,"ddr3_dqs1_l"
  ,"ddr3_dqs1_l"
  ,"ddr3_dqs1_l"
  ,"ddr3_dqs1_l"
  ,"ddr3_dqs1_l"
  ,"ddr3_dqs1_l"
  ,"ddr3_dqs1_l"
  ,"ddr3_error_alert_n"
  ,"ddr3_odt0"
  ,"ddr3_odt0"
  ,"ddr3_odt1"
  ,"ddr3_odt1"
  ,"ddr3_par"
  ,"ddr3_ras_n_a16"
  ,"ddr3_reset_n"
  ,"ddr3_we_n_a14"
  ,"ddr0_a17"
  ,"ddr0_ba2_ten"
  ,"ddr1_a17"
  ,"ddr1_ba2_ten"
  ,"ddr2_a17"
  ,"ddr2_ba2_ten"
  ,"ddr3_a17"
  ,"ddr3_ba2_ten"
  ,"chip_reset_n"
  ,"ddr_pll_bypass"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"gpi_gpio"
  ,"jtg_tck"
  ,"jtg_tdi"
  ,"jtg_tdo"
  ,"jtg_tms"
  ,"jtg_trst_n"
  ,"msc_scan_enable"
  ,"perst0_n"
  ,"perst1_n"
  ,"pll_bypass"
  ,"pll_dcok"
  ,"pll_mul"
  ,"pll_mul"
  ,"pll_mul"
  ,"pll_mul"
  ,"pll_mul"
  ,"pll_mul"
  ,"pnr_pll_mul"
  ,"pnr_pll_mul"
  ,"pnr_pll_mul"
  ,"pnr_pll_mul"
  ,"pnr_pll_mul"
  ,"smi0_mdc"
  ,"smi0_mdio"
  ,"smi1_mdc"
  ,"smi1_mdio"
  ,"tws0_scl"
  ,"tws0_sda"
  ,"tws1_scl"
  ,"tws1_sda"
  ,"uart0_cts_n"
  ,"uart0_rts_n"
  ,"uart0_sin"
  ,"uart0_sout"
  ,"uart1_cts_n"
  ,"uart1_rts_n"
  ,"uart1_sin"
  ,"uart1_sout"
  ,"efuse_pgm_int"
  ,"efuse_pgm_ext"
  ,"spi_ck"
  ,"emmc_cmd"
  ,"emmc_cmd"
  ,"emmc_cmd"
  ,"emmc_dat"
  ,"emmc_dat"
  ,"emmc_dat"
  ,"emmc_dat"
  ,"emmc_dat"
  ,"emmc_dat"
  ,"emmc_dat"
  ,"emmc_dat"
  ,"perst2_n"
  ,"perst3_n"
  ,"chip_reset_out_n"
  ,"thermal_trip_n"
  ]
sigsToStrip :: Set.HashSet B.ByteString
sigsToStrip = Set.fromList
 [ "sb0_refclk_m"
 ,"sb0_refclk_p"
 ,"sb1_refclk_m"
 ,"sb1_refclk_p"
 ,"ncsi_ref_clk"
 ,"tns_pll_byp_clk_h"
 ,"tns_pll_byp_clk_l"
 ,"qlmc_ref_clkn"
 ,"qlmc_ref_clkn"
 ,"qlmc_ref_clkp"
 ,"qlmc_ref_clkp"
 ,"ocic_ref_clkn"
 ,"ocic_ref_clkp"
 ,"oci0_refclkn_nc"
 ,"oci0_refclkp_nc"
 ,"oci1_refclkn_nc"
 ,"oci1_refclkp_nc"
 ,"oci2_refclkn_nc"
 ,"oci2_refclkp_nc"
 ,"oci3_refclkn_nc"
 ,"oci3_refclkp_nc"
 ,"oci4_refclkn_nc"
 ,"oci4_refclkp_nc"
 ,"oci5_refclkn_nc"
 ,"oci5_refclkp_nc"
 ,"ocic_ref_clkn_nc"
 ,"ocic_ref_clkp_nc"
 ,"ddrs_byp_clk_h"
 ,"ddrs_byp_clk_l"
 ,"msc_sys_clkout"
 ,"ddrn_byp_clk_h"
 ,"ddrn_byp_clk_l"
 ,"pll_ref_clk_north"
 ,"pll_ref_clk_south"
 ,"msc_clkout"
 ,"pll_byp_clk_h"
 ,"pll_byp_clk_l"
 ,"pnr_pll_byp_clk_h"
 ,"pnr_pll_byp_clk_l"
 ,"emmc_clk"
 ,"msc_tns_clkout"
 ,"po_msc_clkout"
 ,"po_vrm_i2_clk"
 ]

-- TODO: ++ is slow
headersToList (h@(Scope _ hs):hss) = h:headersToList hs ++ headersToList hss
headersToList             (h:hs)   = h:headersToList hs
headersToList              []      = []

aliasesToStrip :: [Header] -> Set.HashSet B.ByteString
aliasesToStrip = aliasesFromSigs sigsToStrip

aliasesToKeep = aliasesFromSigs sigsToKeep

aliasesFromSigs :: Set.HashSet B.ByteString -> [Header] -> Set.HashSet B.ByteString
aliasesFromSigs sigs =
    Set.fromList .
    map alias .
    filter (flip Set.member sigs . name) .
    filter isWire .
    headersToList
  where
    name  (Wire _ _ nm)  = nm
    alias  (Wire _ al _) = al
    isWire (Wire _ _ _)  = True
    isWire _             = False

filterSig :: Set.HashSet B.ByteString -> B.ByteString -> B.ByteString
filterSig badsigs =
    B.unlines
    . filter (isGood badsigs)
    . B.lines

isGood badsigs l =
  let a = B.head l
      sig = B.drop 1 l
      blank = B.null l
  in
      blank
   || a == '#'
   || a == '$'
   || not (Set.member sig badsigs)

{-# INLINE isKeep #-}
isKeep goodsigs l =
  let (a, sig) = B.splitAt 1 l
      blank = B.null l
  in
      blank
   || a == "#"
   || a == "$"
   || Set.member sig goodsigs


warn :: String -> IO ()
warn s = hPutStrLn stderr $ "Warning: " ++ s

isTimestamp :: B.ByteString -> Bool
isTimestamp a = "#" `B.isPrefixOf` a

-- BUG: some timestamps aren't getting eaten
eatExtraTimestamps :: [B.ByteString] -> [B.ByteString]
eatExtraTimestamps (a:b:cs) =
  if isTimestamp a && isTimestamp b
    then eatExtraTimestamps (b:cs)
    else a : eatExtraTimestamps (b:cs)
eatExtraTimestamps a = a

chunkSize = 64000
-- chunkLines = 4000

-- {-# INLINE filterChunk #-}
filterChunk
 :: Set.HashSet B.ByteString -- ^ signal ids to keep
 -> B.ByteString -- ^ A chunk
 -> B.ByteString
filterChunk keep bs =
      bs `seq` (B.unlines . eatExtraTimestamps . filter (isKeep keep) . B.lines $ bs)

filterChunks :: Int -> [B.ByteString] -> Set.HashSet B.ByteString -> [B.ByteString]
filterChunks nThreads bs keep =
  withStrategy (parBuffer nThreads rdeepseq) . map (filterChunk keep) $ bs

{-# INLINE chunk #-}
chunk :: B.ByteString -> [B.ByteString]
chunk = go
  where
      go "" = []
      go bs = 
        let (first, rest) = B.splitAt chunkSize bs
            (finish, rest') = B.span (/= '\n') rest
        in (B.concat [first, finish]) : go rest'

-- cabal/stack: compile RTS threaded
main :: IO ()
main = do
    cpus <- getNumCapabilities
    when (cpus < 48) $
      warn $ "Only " ++ show cpus ++ " cpus detected. We recommend at least 48."

    f <- B.getContents
    let res = parse parseAllHeaders f
        (hdrs, theRest) =
          case res of
            Fail unconsumed contexts message ->
                error $ "VCD header parse failed: "
                     ++ message
                     ++ "\n---- Contexts ----\n"
                     ++ unlines contexts
                     ++ "\n------------------\n"
                     ++ "\nUnconsumed:\n" ++ Prelude.take 200 (B.unpack unconsumed)
                     ++ "\n...\n"
            Done theRest hdrs -> (hdrs, theRest)
        aliases = aliasesToStrip hdrs
        keep = aliasesToKeep hdrs
        chunks = chunk theRest

    let outputs = filterChunks cpus chunks keep
    mapM_ B.putStr outputs

    -- mapM_ B.putStrLn $ eatExtraTimestamps . filter (isKeep keep) . B.lines $ theRest
    -- print $ parseOnly parseSignal theRest
